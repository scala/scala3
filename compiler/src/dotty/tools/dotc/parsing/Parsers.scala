package dotty.tools
package dotc
package parsing

import scala.annotation.internal.sharable
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.BitSet
import util.{ SourceFile, SourcePosition, NoSourcePosition }
import Tokens._
import Scanners._
import xml.MarkupParsers.MarkupParser
import core._
import Flags._
import Contexts._
import Names._
import NameKinds.WildcardParamName
import ast.{Positioned, Trees}
import ast.Trees._
import StdNames._
import util.Spans._
import Constants._
import Symbols.defn
import ScriptParsers._
import Decorators._
import scala.tasty.util.Chars.isIdentifierStart
import scala.annotation.{tailrec, switch}
import rewrites.Rewrites.patch

object Parsers {

  import ast.untpd._
  import reporting.diagnostic.Message
  import reporting.diagnostic.messages._

  case class OpInfo(operand: Tree, operator: Ident, offset: Offset)

  class ParensCounters {
    private[this] var parCounts = new Array[Int](lastParen - firstParen)

    def count(tok: Token): Int = parCounts(tok - firstParen)
    def change(tok: Token, delta: Int): Unit = parCounts(tok - firstParen) += delta
    def nonePositive: Boolean = parCounts forall (_ <= 0)
  }

  @sharable object Location extends Enumeration {
    val InParens, InBlock, InPattern, ElseWhere: Value = Value
  }

  @sharable object ParamOwner extends Enumeration {
    val Class, Type, TypeParam, Def: Value = Value
  }

  type StageKind = Int
  object StageKind {
    val None = 0
    val Quoted = 1
    val Spliced = 2
  }

  private implicit class AddDeco(val buf: ListBuffer[Tree]) extends AnyVal {
    def +++=(x: Tree) = x match {
      case x: Thicket => buf ++= x.trees
      case x => buf += x
    }
  }

  /** The parse starting point depends on whether the source file is self-contained:
   *  if not, the AST will be supplemented.
   */
  def parser(source: SourceFile)(implicit ctx: Context): Parser =
    if (source.isSelfContained) new ScriptParser(source)
    else new Parser(source)

  abstract class ParserCommon(val source: SourceFile)(implicit ctx: Context) {

    val in: ScannerCommon

    /* ------------- POSITIONS ------------------------------------------- */

    /** Positions tree.
     *  If `t` does not have a span yet, set its span to the given one.
     */
    def atSpan[T <: Positioned](span: Span)(t: T): T =
      if (t.span.isSourceDerived) t else t.withSpan(span.union(t.span))

    def atSpan[T <: Positioned](start: Offset, point: Offset, end: Offset)(t: T): T =
      atSpan(Span(start, end, point))(t)

    /** If the last read offset is strictly greater than `start`, assign tree
     *  the span from `start` to last read offset, with given point.
     *  If the last offset is less than or equal to start, the tree `t` did not
     *  consume any source for its construction. In this case, don't assign a span yet,
     *  but wait for its span to be determined by `setChildSpans` when the
     *  parent node is positioned.
     */
    def atSpan[T <: Positioned](start: Offset, point: Offset)(t: T): T =
      if (in.lastOffset > start) atSpan(start, point, in.lastOffset)(t) else t

    def atSpan[T <: Positioned](start: Offset)(t: T): T =
      atSpan(start, start)(t)

    def startOffset(t: Positioned): Int =
      if (t.span.exists) t.span.start else in.offset

    def pointOffset(t: Positioned): Int =
      if (t.span.exists) t.span.point else in.offset

    def endOffset(t: Positioned): Int =
      if (t.span.exists) t.span.end else in.lastOffset

    def nameStart: Offset =
      if (in.token == BACKQUOTED_IDENT) in.offset + 1 else in.offset

    def sourcePos(off: Int = in.offset): SourcePosition =
      source.atSpan(Span(off))

    /** in.offset, except if this is at a new line, in which case `lastOffset` is preferred. */
    def expectedOffset: Int = {
      val current = sourcePos(in.offset)
      val last = sourcePos(in.lastOffset)
      if (current.line != last.line) in.lastOffset else in.offset
    }

    /* ------------- ERROR HANDLING ------------------------------------------- */
    /** The offset where the last syntax error was reported, or if a skip to a
      *  safepoint occurred afterwards, the offset of the safe point.
      */
    protected var lastErrorOffset : Int = -1

    /** Issue an error at given offset if beyond last error offset
      *  and update lastErrorOffset.
      */
    def syntaxError(msg: => Message, offset: Int = in.offset): Unit =
      if (offset > lastErrorOffset) {
        val length = if (offset == in.offset && in.name != null) in.name.show.length else 0
        syntaxError(msg, Span(offset, offset + length))
        lastErrorOffset = in.offset
      }

    /** Unconditionally issue an error at given span, without
     *  updating lastErrorOffset.
     */
    def syntaxError(msg: => Message, span: Span): Unit =
      ctx.error(msg, source.atSpan(span))

    def unimplementedExpr(implicit ctx: Context): Select =
      Select(Select(rootDot(nme.scala_), nme.Predef), nme.???)
  }

  trait OutlineParserCommon extends ParserCommon {
    def accept(token: Int): Int

    def skipBracesHook(): Option[Tree]
    def skipBraces(): Unit = {
      accept(LBRACE)
      var openBraces = 1
      while (in.token != EOF && openBraces > 0) {
        skipBracesHook() getOrElse {
          if (in.token == LBRACE) openBraces += 1
          else if (in.token == RBRACE) openBraces -= 1
          in.nextToken()
        }
      }
    }
  }

  class Parser(source: SourceFile)(implicit ctx: Context) extends ParserCommon(source) {

    val in: Scanner = new Scanner(source)

    val openParens: ParensCounters = new ParensCounters

    /** This is the general parse entry point.
     *  Overridden by ScriptParser
     */
    def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    }

/* -------------- TOKEN CLASSES ------------------------------------------- */

    def isIdent: Boolean = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT
    def isIdent(name: Name): Boolean = in.token == IDENTIFIER && in.name == name
    def isSimpleLiteral: Boolean = simpleLiteralTokens contains in.token
    def isLiteral: Boolean = literalTokens contains in.token
    def isNumericLit: Boolean = numericLitTokens contains in.token
    def isTemplateIntro: Boolean = templateIntroTokens contains in.token
    def isDclIntro: Boolean = dclIntroTokens contains in.token
    def isStatSeqEnd: Boolean = in.token == RBRACE || in.token == EOF
    def mustStartStat: Boolean = mustStartStatTokens contains in.token

    /** Is current token a hard or soft modifier (in modifier position or not)? */
    def isModifier: Boolean = modifierTokens.contains(in.token) || in.isSoftModifier

    def isBindingIntro: Boolean =
      canStartBindingTokens.contains(in.token) &&
      !in.isSoftModifierInModifierPosition

    def isExprIntro: Boolean =
      if (in.token == IMPLIED) in.lookaheadIn(BitSet(MATCH))
      else (canStartExpressionTokens.contains(in.token) && !in.isSoftModifierInModifierPosition)

    def isDefIntro(allowedMods: BitSet, excludedSoftModifiers: Set[TermName] = Set.empty): Boolean =
      in.token == AT ||
      (defIntroTokens `contains` in.token) ||
      (allowedMods `contains` in.token) ||
      in.isSoftModifierInModifierPosition && !excludedSoftModifiers.contains(in.name)

    def isStatSep: Boolean =
      in.token == NEWLINE || in.token == NEWLINES || in.token == SEMI

    /** A '$' identifier is treated as a splice if followed by a `{`.
     *  A longer identifier starting with `$` is treated as a splice/id combination
     *  in a quoted block '{...'
     */
    def isSplice: Boolean =
      in.token == IDENTIFIER && in.name(0) == '$' && {
        if (in.name.length == 1) in.lookaheadIn(BitSet(LBRACE))
        else (staged & StageKind.Quoted) != 0
      }

/* ------------- ERROR HANDLING ------------------------------------------- */

    /** The offset of the last time when a statement on a new line was definitely
     *  encountered in the current scope or an outer scope.
     */
    private[this] var lastStatOffset = -1

    def setLastStatOffset(): Unit =
      if (mustStartStat && in.isAfterLineEnd())
        lastStatOffset = in.offset

    /** Is offset1 less or equally indented than offset2?
     *  This is the case if the characters between the preceding end-of-line and offset1
     *  are a prefix of the characters between the preceding end-of-line and offset2.
     */
    def isLeqIndented(offset1: Int, offset2: Int): Boolean = {
      def recur(idx1: Int, idx2: Int): Boolean =
        idx1 == offset1 ||
        idx2 < offset2 && source(idx1) == source(idx2) && recur(idx1 + 1, idx2 + 1)
      recur(source.startOfLine(offset1), source.startOfLine(offset2))
    }

    /** Skip on error to next safe point.
     *  Safe points are:
     *   - Closing braces, provided they match an opening brace before the error point.
     *   - Closing parens and brackets, provided they match an opening parent or bracket
     *     before the error point and there are no intervening other kinds of parens.
     *   - Semicolons and newlines, provided there are no intervening braces.
     *   - Definite statement starts on new lines, provided they are not more indented
     *     than the last known statement start before the error point.
     */
    protected def skip(): Unit = {
      val skippedParens = new ParensCounters
      while (true) {
        (in.token: @switch) match {
          case EOF =>
            return
          case SEMI | NEWLINE | NEWLINES =>
            if (skippedParens.count(LBRACE) == 0) return
          case RBRACE =>
            if (openParens.count(LBRACE) > 0 && skippedParens.count(LBRACE) == 0)
              return
            skippedParens.change(LBRACE, -1)
          case RPAREN =>
            if (openParens.count(LPAREN) > 0 && skippedParens.nonePositive)
              return
            skippedParens.change(LPAREN, -1)
          case RBRACKET =>
            if (openParens.count(LBRACKET) > 0 && skippedParens.nonePositive)
              return
            skippedParens.change(LBRACKET, -1)
          case LBRACE =>
            skippedParens.change(LBRACE, + 1)
          case LPAREN =>
            skippedParens.change(LPAREN, + 1)
          case LBRACKET=>
            skippedParens.change(LBRACKET, + 1)
          case _ =>
            if (mustStartStat &&
                in.isAfterLineEnd() &&
                isLeqIndented(in.offset, lastStatOffset max 0))
              return
        }
        in.nextToken()
      }
    }

    def warning(msg: => Message, sourcePos: SourcePosition): Unit =
      ctx.warning(msg, sourcePos)

    def warning(msg: => Message, offset: Int = in.offset): Unit =
      ctx.warning(msg, source.atSpan(Span(offset)))

    def deprecationWarning(msg: => Message, offset: Int = in.offset): Unit =
      ctx.deprecationWarning(msg, source.atSpan(Span(offset)))

    def errorOrMigrationWarning(msg: => Message, offset: Int = in.offset): Unit =
      ctx.errorOrMigrationWarning(msg, source.atSpan(Span(offset)))

    /** Issue an error at current offset that input is incomplete */
    def incompleteInputError(msg: => Message): Unit =
      ctx.incompleteInputError(msg, source.atSpan(Span(in.offset)))

    /** If at end of file, issue an incompleteInputError.
     *  Otherwise issue a syntax error and skip to next safe point.
     */
    def syntaxErrorOrIncomplete(msg: => Message, offset: Int = in.offset): Unit =
      if (in.token == EOF) incompleteInputError(msg)
      else {
        syntaxError(msg, offset)
        skip()
        lastErrorOffset = in.offset
      }

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      *
      * @return The offset at the start of the token to accept
      */
    def accept(token: Int): Int = {
      val offset = in.offset
      if (in.token != token) {
        syntaxErrorOrIncomplete(ExpectedTokenButFound(token, in.token))
      }
      if (in.token == token) in.nextToken()
      offset
    }

    /** semi = nl {nl} | `;'
     *  nl  = `\n' // where allowed
     */
    def acceptStatSep(): Unit = in.token match {
      case NEWLINE | NEWLINES => in.nextToken()
      case _                  => accept(SEMI)
    }

    def acceptStatSepUnlessAtEnd(altEnd: Token = EOF): Unit =
      if (!isStatSeqEnd)
        in.token match {
          case EOF =>
          case `altEnd` =>
          case NEWLINE | NEWLINES => in.nextToken()
          case SEMI => in.nextToken()
          case _ =>
            syntaxError("end of statement expected")
            in.nextToken() // needed to ensure progress; otherwise we might cycle forever
            accept(SEMI)
        }

    def errorTermTree: Literal = atSpan(in.offset) { Literal(Constant(null)) }

    private[this] var inFunReturnType = false
    private def fromWithinReturnType[T](body: => T): T = {
      val saved = inFunReturnType
      try {
        inFunReturnType = true
        body
      } finally inFunReturnType = saved
    }

    /** A flag indicating we are parsing in the annotations of a primary
     *  class constructor
     */
    private[this] var inClassConstrAnnots = false
    private def fromWithinClassConstr[T](body: => T): T = {
      val saved = inClassConstrAnnots
      inClassConstrAnnots = true
      try body
      finally inClassConstrAnnots = saved
    }

    private[this] var inEnum = false
    private def withinEnum[T](body: => T): T = {
      val saved = inEnum
      inEnum = true
      try body
      finally inEnum = saved
    }

    private[this] var staged = StageKind.None
    def withinStaged[T](kind: StageKind)(op: => T): T = {
      val saved = staged
      staged |= kind
      try op
      finally staged = saved
    }

    def migrationWarningOrError(msg: String, offset: Int = in.offset): Unit =
      if (in.isScala2Mode)
        ctx.migrationWarning(msg, source.atSpan(Span(offset)))
      else
        syntaxError(msg, offset)

/* ---------- TREE CONSTRUCTION ------------------------------------------- */

    /** Convert tree to formal parameter list
    */
    def convertToParams(tree: Tree): List[ValDef] = tree match {
      case Parens(t)  => convertToParam(t) :: Nil
      case Tuple(ts)  => ts map (convertToParam(_))
      case t          => convertToParam(t) :: Nil
    }

    /** Convert tree to formal parameter
    */
    def convertToParam(tree: Tree, expected: String = "formal parameter"): ValDef = tree match {
      case id @ Ident(name) =>
        makeParameter(name.asTermName, TypeTree(), isBackquoted = id.isBackquoted).withSpan(tree.span)
      case Typed(id @ Ident(name), tpt) =>
        makeParameter(name.asTermName, tpt, isBackquoted = id.isBackquoted).withSpan(tree.span)
      case Typed(Splice(Ident(name)), tpt) =>
        makeParameter(("$" + name).toTermName, tpt).withSpan(tree.span)
      case _ =>
        syntaxError(s"not a legal $expected", tree.span)
        makeParameter(nme.ERROR, tree)
    }

    /** Convert (qual)ident to type identifier
     */
    def convertToTypeId(tree: Tree): Tree = tree match {
      case id @ Ident(name) =>
        cpy.Ident(id)(name.toTypeName)
      case id @ Select(qual, name) =>
        cpy.Select(id)(qual, name.toTypeName)
      case _ =>
        syntaxError(IdentifierExpected(tree.show), tree.span)
        tree
    }

/* --------------- PLACEHOLDERS ------------------------------------------- */

    /** The implicit parameters introduced by `_` in the current expression.
     *  Parameters appear in reverse order.
     */
    var placeholderParams: List[ValDef] = Nil

    def checkNoEscapingPlaceholders[T](op: => T): T = {
      val savedPlaceholderParams = placeholderParams
      placeholderParams = Nil

      try op
      finally {
        placeholderParams match {
          case vd :: _ => syntaxError(UnboundPlaceholderParameter(), vd.span)
          case _ =>
        }
        placeholderParams = savedPlaceholderParams
      }
    }

    def isWildcard(t: Tree): Boolean = t match {
      case Ident(name1) => placeholderParams.nonEmpty && name1 == placeholderParams.head.name
      case Typed(t1, _) => isWildcard(t1)
      case Annotated(t1, _) => isWildcard(t1)
      case Parens(t1) => isWildcard(t1)
      case _ => false
    }

    def isWildcardType(t: Tree): Boolean = t match {
      case t: TypeBoundsTree => true
      case Parens(t1) => isWildcardType(t1)
      case _ => false
    }

    def rejectWildcardType(t: Tree, fallbackTree: Tree = scalaAny): Tree =
      if (isWildcardType(t)) {
        syntaxError(UnboundWildcardType(), t.span)
        fallbackTree
      }
      else t

/* -------------- XML ---------------------------------------------------- */

    /** The markup parser.
     *  The first time this lazy val is accessed, we assume we were trying to parse an XML literal.
     *  The current position is recorded for later error reporting if it turns out
     *  that we don't have scala-xml on the compilation classpath.
     */
    lazy val xmlp: xml.MarkupParsers.MarkupParser = {
      myFirstXmlPos = source.atSpan(Span(in.offset))
      new MarkupParser(this, true)
    }

    /** The position of the first XML literal encountered while parsing,
     *  NoSourcePosition if there were no XML literals.
     */
    def firstXmlPos: SourcePosition = myFirstXmlPos
    private[this] var myFirstXmlPos: SourcePosition = NoSourcePosition

    object symbXMLBuilder extends xml.SymbolicXMLBuilder(this, true) // DEBUG choices

    def xmlLiteral() : Tree = xmlp.xLiteral
    def xmlLiteralPattern() : Tree = xmlp.xLiteralPattern

/* -------- COMBINATORS -------------------------------------------------------- */

    def enclosed[T](tok: Token, body: => T): T = {
      accept(tok)
      openParens.change(tok, 1)
      try body
      finally {
        accept(tok + 1)
        openParens.change(tok, -1)
      }
    }

    def inParens[T](body: => T): T = enclosed(LPAREN, body)
    def inBraces[T](body: => T): T = enclosed(LBRACE, body)
    def inBrackets[T](body: => T): T = enclosed(LBRACKET, body)

    def inDefScopeBraces[T](body: => T): T = {
      val saved = lastStatOffset
      try inBraces(body)
      finally lastStatOffset = saved
    }

    /** part { `separator` part }
     */
    def tokenSeparated[T](separator: Int, part: () => T): List[T] = {
      val ts = new ListBuffer[T] += part()
      while (in.token == separator) {
        in.nextToken()
        ts += part()
      }
      ts.toList
    }

    def commaSeparated[T](part: () => T): List[T] = tokenSeparated(COMMA, part)

/* --------- OPERAND/OPERATOR STACK --------------------------------------- */

    var opStack: List[OpInfo] = Nil

    def checkAssoc(offset: Token, op1: Name, op2: Name, op2LeftAssoc: Boolean): Unit =
      if (isLeftAssoc(op1) != op2LeftAssoc)
        syntaxError(MixedLeftAndRightAssociativeOps(op1, op2, op2LeftAssoc), offset)

    def reduceStack(base: List[OpInfo], top: Tree, prec: Int, leftAssoc: Boolean, op2: Name, isType: Boolean): Tree = {
      if (opStack != base && precedence(opStack.head.operator.name) == prec)
        checkAssoc(opStack.head.offset, opStack.head.operator.name, op2, leftAssoc)
      def recur(top: Tree): Tree = {
        if (opStack == base) top
        else {
          val opInfo = opStack.head
          val opPrec = precedence(opInfo.operator.name)
          if (prec < opPrec || leftAssoc && prec == opPrec) {
            opStack = opStack.tail
            recur {
              atSpan(opInfo.operator.span union opInfo.operand.span union top.span) {
                InfixOp(opInfo.operand, opInfo.operator, top)
              }
            }
          }
          else top
        }
      }
      recur(top)
    }

    /**   operand { infixop operand | ‘given’ (operand | ParArgumentExprs) } [postfixop],
     *
     *  respecting rules of associativity and precedence.
     *  @param isOperator    the current token counts as an operator.
     *  @param maybePostfix  postfix operators are allowed.
     */
    def infixOps(
        first: Tree, canStartOperand: Token => Boolean, operand: () => Tree,
        isType: Boolean = false,
        isOperator: => Boolean = true,
        maybePostfix: Boolean = false): Tree = {
      val base = opStack

      def recur(top: Tree): Tree =
        if (isIdent && isOperator) {
          val op = if (isType) typeIdent() else termIdent()
          val top1 = reduceStack(base, top, precedence(op.name), isLeftAssoc(op.name), op.name, isType)
          opStack = OpInfo(top1, op, in.offset) :: opStack
          newLineOptWhenFollowing(canStartOperand)
          if (maybePostfix && !canStartOperand(in.token)) {
            val topInfo = opStack.head
            opStack = opStack.tail
            val od = reduceStack(base, topInfo.operand, 0, true, in.name, isType)
            atSpan(startOffset(od), topInfo.offset) {
              PostfixOp(od, topInfo.operator)
            }
          }
          else recur(operand())
        }
        else if (in.token == GIVEN && !isType) {
          val top1 = reduceStack(base, top, minInfixPrec, leftAssoc = true, nme.WITHkw, isType)
          assert(opStack `eq` base)
          recur(applyGiven(top1, operand))
        }
        else reduceStack(base, top, minPrec, leftAssoc = true, in.name, isType)

      recur(first)
    }

    def applyGiven(t: Tree, operand: () => Tree): Tree =
      atSpan(startOffset(t), in.offset) {
        in.nextToken()
        val args = if (in.token == LPAREN) parArgumentExprs() else operand() :: Nil
        Apply(t, args)
      }.setGivenApply()

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

    /** Accept identifier and return its name as a term name. */
    def ident(): TermName =
      if (isIdent) {
        val name = in.name
        in.nextToken()
        name
      } else {
        syntaxErrorOrIncomplete(ExpectedTokenButFound(IDENTIFIER, in.token))
        nme.ERROR
      }

    /** Accept identifier and return Ident with its name as a term name. */
    def termIdent(): Ident =
      makeIdent(in.token, in.offset, ident())

    /** Accept identifier and return Ident with its name as a type name. */
    def typeIdent(): Ident =
      makeIdent(in.token, in.offset, ident().toTypeName)

    private def makeIdent(tok: Token, offset: Offset, name: Name) = {
      val tree =
        if (tok == BACKQUOTED_IDENT) BackquotedIdent(name)
        else Ident(name)

      // Make sure that even trees with parsing errors have a offset that is within the offset
      val errorOffset = offset min (in.lastOffset - 1)
      if (tree.name == nme.ERROR && tree.span == NoSpan) tree.withSpan(Span(errorOffset, errorOffset))
      else atSpan(offset)(tree)
    }

    def wildcardIdent(): Ident =
      atSpan(accept(USCORE)) { Ident(nme.WILDCARD) }

    /** Accept identifier acting as a selector on given tree `t`. */
    def selector(t: Tree): Tree =
      atSpan(startOffset(t), in.offset) { Select(t, ident()) }

    /** Selectors ::= id { `.' id }
     *
     *  Accept `.' separated identifiers acting as a selectors on given tree `t`.
     *  @param finish   An alternative parse in case the next token is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
    def selectors(t: Tree, finish: Tree => Tree): Tree = {
      val t1 = finish(t)
      if (t1 ne t) t1 else dotSelectors(selector(t), finish)
    }

    /** DotSelectors ::= { `.' id }
     *
     *  Accept `.' separated identifiers acting as a selectors on given tree `t`.
     *  @param finish   An alternative parse in case the token following a `.' is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
     def dotSelectors(t: Tree, finish: Tree => Tree = id): Tree =
      if (in.token == DOT) { in.nextToken(); selectors(t, finish) }
      else t

    private val id: Tree => Tree = x => x

    /** Path       ::= StableId
     *              |  [id `.'] this
     *
     *  @param thisOK   If true, the path can end with the keyword `this`.
     *                  If false, another selection is required after the `this`.
     *  @param finish   An alternative parse in case the token following a `.' is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
    def path(thisOK: Boolean, finish: Tree => Tree = id): Tree = {
      val start = in.offset
      def handleThis(qual: Ident) = {
        in.nextToken()
        val t = atSpan(start) { This(qual) }
        if (!thisOK && in.token != DOT) syntaxError(DanglingThisInPath(), t.span)
        dotSelectors(t, finish)
      }
      def handleSuper(qual: Ident) = {
        in.nextToken()
        val mix = mixinQualifierOpt()
        val t = atSpan(start) { Super(This(qual), mix) }
        accept(DOT)
        dotSelectors(selector(t), finish)
      }
      if (in.token == THIS) handleThis(EmptyTypeIdent)
      else if (in.token == SUPER) handleSuper(EmptyTypeIdent)
      else {
        val t = termIdent()
        if (in.token == DOT) {
          def qual = cpy.Ident(t)(t.name.toTypeName)
          in.nextToken()
          if (in.token == THIS) handleThis(qual)
          else if (in.token == SUPER) handleSuper(qual)
          else selectors(t, finish)
        }
        else t
      }
    }

    /** MixinQualifier ::= `[' id `]'
    */
    def mixinQualifierOpt(): Ident =
      if (in.token == LBRACKET) inBrackets(atSpan(in.offset) { typeIdent() })
      else EmptyTypeIdent

    /** StableId ::= id
     *            |  Path `.' id
     *            |  [id '.'] super [`[' id `]']`.' id
     */
    def stableId(): Tree =
      path(thisOK = false)

    /** QualId ::= id {`.' id}
    */
    def qualId(): Tree = dotSelectors(termIdent())

    /** SimpleExpr    ::= literal
     *                  | 'id | 'this | 'true | 'false | 'null
     *                  | null
     *  @param negOffset   The offset of a preceding `-' sign, if any.
     *                     If the literal is not negated, negOffset = in.offset.
     */
    def literal(negOffset: Int = in.offset, inPattern: Boolean = false, inStringInterpolation: Boolean = false): Tree = {

      def literalOf(token: Token): Literal = {
        val isNegated = negOffset < in.offset
        val value = token match {
          case CHARLIT                => in.charVal
          case INTLIT                 => in.intVal(isNegated).toInt
          case LONGLIT                => in.intVal(isNegated)
          case FLOATLIT               => in.floatVal(isNegated).toFloat
          case DOUBLELIT              => in.doubleVal(isNegated)
          case STRINGLIT | STRINGPART => in.strVal
          case TRUE                   => true
          case FALSE                  => false
          case NULL                   => null
          case _                      =>
            syntaxErrorOrIncomplete(IllegalLiteral())
            null
        }
        Literal(Constant(value))
      }

      if (inStringInterpolation) {
        val t = in.token match {
          case STRINGLIT | STRINGPART =>
            val value = in.strVal
            atSpan(negOffset, negOffset, negOffset + value.length) { Literal(Constant(value)) }
          case _ =>
            syntaxErrorOrIncomplete(IllegalLiteral())
            atSpan(negOffset) { Literal(Constant(null)) }
        }
        in.nextToken()
        t
      }
      else atSpan(negOffset) {
        if (in.token == QUOTEID) {
          if ((staged & StageKind.Spliced) != 0 && isIdentifierStart(in.name(0))) {
            val t = atSpan(in.offset + 1) {
              val tok = in.toToken(in.name)
              tok match {
                case TRUE | FALSE | NULL => literalOf(tok)
                case THIS => This(EmptyTypeIdent)
                case _ => Ident(in.name)
              }
            }
            in.nextToken()
            Quote(t)
          }
          else {
            migrationWarningOrError(em"""symbol literal '${in.name} is no longer supported,
                                        |use a string literal "${in.name}" or an application Symbol("${in.name}") instead,
                                        |or enclose in braces '{${in.name}} if you want a quoted expression.""")
            if (in.isScala2Mode) {
              patch(source, Span(in.offset, in.offset + 1), "Symbol(\"")
              patch(source, Span(in.charOffset - 1), "\")")
            }
            atSpan(in.skipToken()) { SymbolLit(in.strVal) }
          }
        }
        else if (in.token == INTERPOLATIONID) interpolatedString(inPattern)
        else {
          val t = literalOf(in.token)
          in.nextToken()
          t
        }
      }
    }

    private def interpolatedString(inPattern: Boolean = false): Tree = atSpan(in.offset) {
      val segmentBuf = new ListBuffer[Tree]
      val interpolator = in.name
      val isTripleQuoted =
        in.charOffset + 1 < in.buf.length &&
        in.buf(in.charOffset) == '"' &&
        in.buf(in.charOffset + 1) == '"'
      in.nextToken()
      def nextSegment(literalOffset: Offset) =
        segmentBuf += Thicket(
            literal(literalOffset, inPattern = inPattern, inStringInterpolation = true),
            atSpan(in.offset) {
              if (in.token == IDENTIFIER)
                termIdent()
              else if (in.token == USCORE && inPattern) {
                in.nextToken()
                Ident(nme.WILDCARD)
              }
              else if (in.token == THIS) {
                in.nextToken()
                This(EmptyTypeIdent)
              }
              else if (in.token == LBRACE)
                if (inPattern) Block(Nil, inBraces(pattern()))
                else expr()
              else {
                ctx.error(InterpolatedStringError(), source.atSpan(Span(in.offset)))
                EmptyTree
              }
            })

      if (in.token == STRINGPART)
        nextSegment(in.offset + (if (isTripleQuoted) 3 else 1))
      while (in.token == STRINGPART)
        nextSegment(in.offset)
      if (in.token == STRINGLIT)
        segmentBuf += literal(inPattern = inPattern, negOffset = in.offset, inStringInterpolation = true)

      InterpolatedString(interpolator, segmentBuf.toList)
    }

/* ------------- NEW LINES ------------------------------------------------- */

    def newLineOpt(): Unit = {
      if (in.token == NEWLINE) in.nextToken()
    }

    def newLinesOpt(): Unit = {
      if (in.token == NEWLINE || in.token == NEWLINES)
        in.nextToken()
    }

    def newLineOptWhenFollowedBy(token: Int): Unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && in.next.token == token) newLineOpt()
    }

    def newLineOptWhenFollowing(p: Int => Boolean): Unit = {
      // note: next is defined here because current == NEWLINE
      if (in.token == NEWLINE && p(in.next.token)) newLineOpt()
    }

/* ------------- TYPES ------------------------------------------------------ */
    /** Same as [[typ]], but if this results in a wildcard it emits a syntax error and
     *  returns a tree for type `Any` instead.
     */
    def toplevelTyp(): Tree = rejectWildcardType(typ())

    /** Type        ::=  FunType
     *                |  HkTypeParamClause ‘=>>’ Type
     *                |  MatchType
     *                |  InfixType
     *  FunType     ::=  { 'erased' | 'given' } (MonoFunType | PolyFunType)
     *  MonoFunType ::=  FunArgTypes ‘=>’ Type
     *  PolyFunType ::=  HKTypeParamClause '=>' Type
     *  FunArgTypes ::=  InfixType
     *                |  `(' [ FunArgType {`,' FunArgType } ] `)'
     *                |  '(' TypedFunParam {',' TypedFunParam } ')'
     */
    def typ(): Tree = {
      val start = in.offset
      val imods = modifiers(funTypeMods)
      def functionRest(params: List[Tree]): Tree =
        atSpan(start, accept(ARROW)) {
          val t = typ()
          if (imods.is(Given | Erased)) new FunctionWithMods(params, t, imods)
          else Function(params, t)
        }
      def funArgTypesRest(first: Tree, following: () => Tree) = {
        val buf = new ListBuffer[Tree] += first
        while (in.token == COMMA) {
          in.nextToken()
          buf += following()
        }
        buf.toList
      }
      var isValParamList = false

      val t =
        if (in.token == LPAREN) {
          in.nextToken()
          if (in.token == RPAREN) {
            in.nextToken()
            functionRest(Nil)
          }
          else {
            openParens.change(LPAREN, 1)
            val paramStart = in.offset
            val ts = funArgType() match {
              case Ident(name) if name != tpnme.WILDCARD && in.token == COLON =>
                isValParamList = true
                funArgTypesRest(
                    typedFunParam(paramStart, name.toTermName, imods),
                    () => typedFunParam(in.offset, ident(), imods))
              case t =>
                funArgTypesRest(t, funArgType)
            }
            openParens.change(LPAREN, -1)
            accept(RPAREN)
            if (isValParamList || in.token == ARROW)
              functionRest(ts)
            else {
              val ts1 =
                for (t <- ts) yield {
                  t match {
                    case t@ByNameTypeTree(t1) =>
                      syntaxError(ByNameParameterNotSupported(t), t.span)
                      t1
                    case _ =>
                      t
                  }
                }
              val tuple = atSpan(start) { makeTupleOrParens(ts1) }
              infixTypeRest(
                refinedTypeRest(
                  withTypeRest(
                    annotTypeRest(
                      simpleTypeRest(tuple)))))
            }
          }
        }
        else if (in.token == LBRACKET) {
          val start = in.offset
          val tparams = typeParamClause(ParamOwner.TypeParam)
          if (in.token == TLARROW)
            atSpan(start, in.skipToken())(LambdaTypeTree(tparams, toplevelTyp()))
          else if (in.token == ARROW) {
            val arrowOffset = in.skipToken()
            val body = toplevelTyp()
            atSpan(start, arrowOffset) {
              body match {
                case _: Function => PolyFunction(tparams, body)
                case _ =>
                  syntaxError("Implementation restriction: polymorphic function types must have a value parameter", arrowOffset)
                  Ident(nme.ERROR.toTypeName)
              }
            }
          } else { accept(TLARROW); typ() }
        }
        else infixType()

      in.token match {
        case ARROW => functionRest(t :: Nil)
        case MATCH => matchType(t)
        case FORSOME => syntaxError(ExistentialTypesNoLongerSupported()); t
        case _ =>
          if (imods.is(ImplicitOrGiven) && !t.isInstanceOf[FunctionWithMods])
            syntaxError("Types with implicit keyword can only be function types `implicit (...) => ...`", implicitKwPos(start))
          if (imods.is(Erased) && !t.isInstanceOf[FunctionWithMods])
            syntaxError("Types with erased keyword can only be function types `erased (...) => ...`", implicitKwPos(start))
          t
      }
    }

    private def implicitKwPos(start: Int): Span =
      Span(start, start + nme.IMPLICITkw.asSimpleName.length)

    /** TypedFunParam   ::= id ':' Type */
    def typedFunParam(start: Offset, name: TermName, mods: Modifiers = EmptyModifiers): Tree = atSpan(start) {
      accept(COLON)
      makeParameter(name, typ(), mods | Param)
    }

    /** InfixType ::= RefinedType {id [nl] refinedType}
     */
    def infixType(): Tree = infixTypeRest(refinedType())

    /** Is current ident a `*`, and is it followed by a `)` or `,`? */
    def isPostfixStar: Boolean =
      in.name == nme.raw.STAR && in.lookaheadIn(BitSet(RPAREN, COMMA))

    def infixTypeRest(t: Tree): Tree =
      infixOps(t, canStartTypeTokens, refinedType, isType = true, isOperator = !isPostfixStar)

    /** RefinedType        ::=  WithType {Annotation | [nl] Refinement}
     */
    val refinedType: () => Tree = () => refinedTypeRest(withType())

    def refinedTypeRest(t: Tree): Tree = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) refinedTypeRest(atSpan(startOffset(t)) { RefinedTypeTree(rejectWildcardType(t), refinement()) })
      else t
    }

    /** WithType ::= AnnotType {`with' AnnotType}    (deprecated)
     */
    def withType(): Tree = withTypeRest(annotType())

    def withTypeRest(t: Tree): Tree =
      if (in.token == WITH) {
        if (ctx.settings.strict.value)
          deprecationWarning(DeprecatedWithOperator())
        in.nextToken()
        makeAndType(t, withType())
      }
      else t

    /** AnnotType ::= SimpleType {Annotation}
     */
    def annotType(): Tree = annotTypeRest(simpleType())

    def annotTypeRest(t: Tree): Tree =
      if (in.token == AT)
        annotTypeRest(atSpan(startOffset(t)) {
          Annotated(rejectWildcardType(t), annot())
        })
      else t

    /** The block in a quote or splice */
    def stagedBlock() =
      inDefScopeBraces(block()) match {
        case t @ Block(Nil, expr) if !expr.isEmpty => expr
        case t => t
      }

    /** SimpleEpxr  ::=  spliceId | ‘$’ ‘{’ Block ‘}’)
     *  SimpleType  ::=  spliceId | ‘$’ ‘{’ Block ‘}’)
     */
    def splice(isType: Boolean): Tree =
      atSpan(in.offset) {
        val expr =
          if (in.name.length == 1) {
            in.nextToken()
            withinStaged(StageKind.Spliced)(stagedBlock())
          }
          else atSpan(in.offset + 1) {
            val id = Ident(in.name.drop(1))
            in.nextToken()
            id
          }
        if (isType) TypSplice(expr) else Splice(expr)
      }

    /** SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' id
     *                     |  StableId
     *                     |  Path `.' type
     *                     |  `(' ArgTypes `)'
     *                     |  `_' TypeBounds
     *                     |  Refinement
     *                     |  Literal
     *                     |  ‘$’ ‘{’ Block ‘}’
     */
    def simpleType(): Tree = simpleTypeRest {
      if (in.token == LPAREN)
        atSpan(in.offset) {
          makeTupleOrParens(inParens(argTypes(namedOK = false, wildOK = true)))
        }
      else if (in.token == LBRACE)
        atSpan(in.offset) { RefinedTypeTree(EmptyTree, refinement()) }
      else if (isSimpleLiteral) { SingletonTypeTree(literal()) }
      else if (isIdent(nme.raw.MINUS) && in.lookaheadIn(numericLitTokens)) {
        val start = in.offset
        in.nextToken()
        SingletonTypeTree(literal(negOffset = start))
      }
      else if (in.token == USCORE) {
        if (ctx.settings.strict.value) {
          deprecationWarning(em"`_` is deprecated for wildcard arguments of types: use `?` instead")
          patch(source, Span(in.offset, in.offset + 1), "?")
        }
        val start = in.skipToken()
        typeBounds().withSpan(Span(start, in.lastOffset, start))
      }
      else if (isIdent(nme.?)) {
        val start = in.skipToken()
        typeBounds().withSpan(Span(start, in.lastOffset, start))
      }
      else if (isIdent(nme.*) && ctx.settings.YkindProjector.value) {
      	syntaxError("`*` placeholders are not implemented yet")
        typeIdent()
      }
      else if (isSplice)
        splice(isType = true)
      else path(thisOK = false, handleSingletonType) match {
        case r @ SingletonTypeTree(_) => r
        case r => convertToTypeId(r)
      }
    }

    val handleSingletonType: Tree => Tree = t =>
      if (in.token == TYPE) {
        in.nextToken()
        atSpan(startOffset(t)) { SingletonTypeTree(t) }
      } else t

    private def simpleTypeRest(t: Tree): Tree = in.token match {
      case HASH => simpleTypeRest(typeProjection(t))
      case LBRACKET => simpleTypeRest(atSpan(startOffset(t)) {
        AppliedTypeTree(rejectWildcardType(t), typeArgs(namedOK = false, wildOK = true)) })
      case _ => t
    }

    private def typeProjection(t: Tree): Tree = {
      accept(HASH)
      val id = typeIdent()
      atSpan(startOffset(t), startOffset(id)) { Select(t, id.name) }
    }

    /**   ArgTypes          ::=  Type {`,' Type}
     *                        |  NamedTypeArg {`,' NamedTypeArg}
     *    NamedTypeArg      ::=  id `=' Type
     */
    def argTypes(namedOK: Boolean, wildOK: Boolean): List[Tree] = {

      def argType() = {
        val t = typ()
        if (wildOK) t else rejectWildcardType(t)
      }

      def namedTypeArg() = {
        val name = ident()
        accept(EQUALS)
        NamedArg(name.toTypeName, argType())
      }

      def otherArgs(first: Tree, arg: () => Tree): List[Tree] = {
        val rest =
          if (in.token == COMMA) {
            in.nextToken()
            commaSeparated(arg)
          }
          else Nil
        first :: rest
      }
      if (namedOK && in.token == IDENTIFIER)
        argType() match {
          case Ident(name) if in.token == EQUALS =>
            in.nextToken()
            otherArgs(NamedArg(name, argType()), () => namedTypeArg())
          case firstArg =>
            otherArgs(firstArg, () => argType())
        }
      else commaSeparated(() => argType())
    }

    /** FunArgType ::=  Type | `=>' Type
     */
    val funArgType: () => Tree = () =>
      if (in.token == ARROW) atSpan(in.skipToken()) { ByNameTypeTree(typ()) }
      else typ()

    /** ParamType ::= [`=>'] ParamValueType
     */
    def paramType(): Tree =
      if (in.token == ARROW) atSpan(in.skipToken()) { ByNameTypeTree(paramValueType()) }
      else paramValueType()

    /** ParamValueType ::= Type [`*']
     */
    def paramValueType(): Tree = {
      val t = toplevelTyp()
      if (isIdent(nme.raw.STAR)) {
        in.nextToken()
        atSpan(startOffset(t)) { PostfixOp(t, Ident(tpnme.raw.STAR)) }
      } else t
    }

    /** TypeArgs      ::= `[' Type {`,' Type} `]'
     *  NamedTypeArgs ::= `[' NamedTypeArg {`,' NamedTypeArg} `]'
     */
    def typeArgs(namedOK: Boolean, wildOK: Boolean): List[Tree] = inBrackets(argTypes(namedOK, wildOK))

    /** Refinement ::= `{' RefineStatSeq `}'
     */
    def refinement(): List[Tree] = inBraces(refineStatSeq())

    /** TypeBounds ::= [`>:' Type] [`<:' Type]
     */
    def typeBounds(): TypeBoundsTree =
      atSpan(in.offset) { TypeBoundsTree(bound(SUPERTYPE), bound(SUBTYPE)) }

    private def bound(tok: Int): Tree =
      if (in.token == tok) { in.nextToken(); toplevelTyp() }
      else EmptyTree

    /** TypeParamBounds   ::=  TypeBounds {`<%' Type} {`:' Type}
     */
    def typeParamBounds(pname: TypeName): Tree = {
      val t = typeBounds()
      val cbs = contextBounds(pname)
      if (cbs.isEmpty) t
      else atSpan((t.span union cbs.head.span).start) { ContextBounds(t, cbs) }
    }

    def contextBounds(pname: TypeName): List[Tree] = in.token match {
      case COLON =>
        atSpan(in.skipToken()) {
          AppliedTypeTree(toplevelTyp(), Ident(pname))
        } :: contextBounds(pname)
      case VIEWBOUND =>
        errorOrMigrationWarning("view bounds `<%' are deprecated, use a context bound `:' instead")
        atSpan(in.skipToken()) {
          Function(Ident(pname) :: Nil, toplevelTyp())
        } :: contextBounds(pname)
      case _ =>
        Nil
    }

    def typedOpt(): Tree =
      if (in.token == COLON) { in.nextToken(); toplevelTyp() }
      else TypeTree().withSpan(Span(in.lastOffset))

    def typeDependingOn(location: Location.Value): Tree =
      if (location == Location.InParens) typ()
      else if (location == Location.InPattern) refinedType()
      else infixType()

/* ----------- EXPRESSIONS ------------------------------------------------ */

    /** EqualsExpr ::= `=' Expr
     */
    def equalsExpr(): Tree = {
      accept(EQUALS)
      expr()
    }

    def condExpr(altToken: Token): Tree = {
      if (in.token == LPAREN) {
        val t = atSpan(in.offset) { Parens(inParens(exprInParens())) }
        if (in.token == altToken) in.nextToken()
        t
      } else {
        val t = expr()
        accept(altToken)
        t
      }
    }

    /** Expr              ::=  [ClosureMods] FunParams =>' Expr
     *                      |  Expr1
     *  FunParams         ::=  Bindings
     *                      |  id
     *                      |  `_'
     *  ExprInParens      ::=  PostfixExpr `:' Type
     *                      |  Expr
     *  BlockResult       ::=  [ClosureMods] FunParams =>' Block
     *                      |  Expr1
     *  Expr1             ::=  [‘inline’] `if' `(' Expr `)' {nl} Expr [[semi] else Expr]
     *                      |  [‘inline’] `if' Expr `then' Expr [[semi] else Expr]
     *                      |  `while' `(' Expr `)' {nl} Expr
     *                      |  `while' Expr `do' Expr
     *                      |  `do' Expr [semi] `while' Expr
     *                      |  `try' Expr Catches [`finally' Expr]
     *                      |  `try' Expr [`finally' Expr]
     *                      |  `throw' Expr
     *                      |  `return' [Expr]
     *                      |  ForExpr
     *                      |  HkTypeParamClause ‘=>’ Expr
     *                      |  [SimpleExpr `.'] id `=' Expr
     *                      |  SimpleExpr1 ArgumentExprs `=' Expr
     *                      |  Expr2
     *                      |  [‘inline’] Expr2 `match' `{' CaseClauses `}'
     *                      |  `delegate' `match' `{' ImplicitCaseClauses `}'
     *  Bindings          ::=  `(' [Binding {`,' Binding}] `)'
     *  Binding           ::=  (id | `_') [`:' Type]
     *  Expr2             ::=  PostfixExpr [Ascription]
     *  Ascription        ::=  `:' InfixType
     *                      |  `:' Annotation {Annotation}
     *                      |  `:' `_' `*'
     */
    val exprInParens: () => Tree = () => expr(Location.InParens)

    def expr(): Tree = expr(Location.ElseWhere)

    def expr(location: Location.Value): Tree = {
      val start = in.offset
      if (closureMods.contains(in.token)) {
        val imods = modifiers(closureMods)
        if (in.token == MATCH) impliedMatch(start, imods)
        else implicitClosure(start, location, imods)
      }
      else if(in.token == IMPLIED) {
        in.nextToken()
        if (in.token == MATCH)
          impliedMatch(start, EmptyModifiers)
        else {
          syntaxError("`match` expected")
          EmptyTree
        }
      }
      else {
        val saved = placeholderParams
        placeholderParams = Nil

        def wrapPlaceholders(t: Tree) = try
          if (placeholderParams.isEmpty) t
          else new WildcardFunction(placeholderParams.reverse, t)
        finally placeholderParams = saved

        val t = expr1(location)
        if (in.token == ARROW) {
          placeholderParams = Nil // don't interpret `_' to the left of `=>` as placeholder
          wrapPlaceholders(closureRest(start, location, convertToParams(t)))
        }
        else if (isWildcard(t)) {
          placeholderParams = placeholderParams ::: saved
          t
        }
        else wrapPlaceholders(t)
      }
    }

    def expr1(location: Location.Value = Location.ElseWhere): Tree = in.token match {
      case IF =>
        ifExpr(in.offset, If)
      case WHILE =>
        atSpan(in.skipToken()) {
          val cond = condExpr(DO)
          newLinesOpt()
          val body = expr()
          WhileDo(cond, body)
        }
      case DO =>
        atSpan(in.skipToken()) {
          val body = expr()
          if (isStatSep) in.nextToken()
          accept(WHILE)
          val cond = expr()
          DoWhile(body, cond)
        }
      case TRY =>
        val tryOffset = in.offset
        atSpan(in.skipToken()) {
          val body = expr()
          val (handler, handlerStart) =
            if (in.token == CATCH) {
              val span = in.offset
              in.nextToken()
              (expr(), span)
            } else (EmptyTree, -1)

          handler match {
            case Block(Nil, EmptyTree) =>
              assert(handlerStart != -1)
              syntaxError(
                EmptyCatchBlock(body),
                Span(handlerStart, endOffset(handler))
              )
            case _ =>
          }

          val finalizer =
            if (in.token == FINALLY) { in.nextToken(); expr() }
            else {
              if (handler.isEmpty) warning(
                EmptyCatchAndFinallyBlock(body),
                source.atSpan(Span(tryOffset, endOffset(body)))
              )
              EmptyTree
            }
          ParsedTry(body, handler, finalizer)
        }
      case THROW =>
        atSpan(in.skipToken()) { Throw(expr()) }
      case RETURN =>
        atSpan(in.skipToken()) { Return(if (isExprIntro) expr() else EmptyTree, EmptyTree) }
      case FOR =>
        forExpr()
      case LBRACKET =>
        val start = in.offset
        val tparams = typeParamClause(ParamOwner.TypeParam)
        val arrowOffset = accept(ARROW)
        val body = expr()
        atSpan(start, arrowOffset) {
          body match {
            case _: Function => PolyFunction(tparams, body)
            case _ =>
              syntaxError("Implementation restriction: polymorphic function literals must have a value parameter", arrowOffset)
              errorTermTree
          }
        }
      case _ =>
        if (isIdent(nme.inline) && !in.inModifierPosition() && in.lookaheadIn(canStartExpressionTokens)) {
          val start = in.skipToken()
          in.token match {
            case IF =>
              ifExpr(start, InlineIf)
            case _ =>
              val t = postfixExpr()
              if (in.token == MATCH) matchExpr(t, start, InlineMatch)
              else {
                syntaxErrorOrIncomplete(i"`match` or `if` expected but ${in.token} found")
                t
              }
          }
        }
        else expr1Rest(postfixExpr(), location)
    }

    def expr1Rest(t: Tree, location: Location.Value): Tree = in.token match {
      case EQUALS =>
         t match {
           case Ident(_) | Select(_, _) | Apply(_, _) =>
             atSpan(startOffset(t), in.skipToken()) { Assign(t, expr()) }
           case _ =>
             t
         }
      case COLON =>
        in.nextToken()
        val t1 = ascription(t, location)
        if (in.token == MATCH) expr1Rest(t1, location) else t1
      case MATCH =>
        matchExpr(t, startOffset(t), Match)
      case _ =>
        t
    }

    def ascription(t: Tree, location: Location.Value): Tree = atSpan(startOffset(t)) {
      in.token match {
        case USCORE =>
          val uscoreStart = in.skipToken()
          if (isIdent(nme.raw.STAR)) {
            in.nextToken()
            if (in.token != RPAREN) syntaxError(SeqWildcardPatternPos(), uscoreStart)
            Typed(t, atSpan(uscoreStart) { Ident(tpnme.WILDCARD_STAR) })
          } else {
            syntaxErrorOrIncomplete(IncorrectRepeatedParameterSyntax())
            t
          }
        case AT if location != Location.InPattern =>
          (t /: annotations())(Annotated)
        case _ =>
          val tpt = typeDependingOn(location)
          if (isWildcard(t) && location != Location.InPattern) {
            val vd :: rest = placeholderParams
            placeholderParams =
              cpy.ValDef(vd)(tpt = tpt).withSpan(vd.span.union(tpt.span)) :: rest
          }
          Typed(t, tpt)
      }
    }

    /**    `if' `(' Expr `)' {nl} Expr [[semi] else Expr]
     *     `if' Expr `then' Expr [[semi] else Expr]
     */
    def ifExpr(start: Offset, mkIf: (Tree, Tree, Tree) => If): If =
      atSpan(start, in.skipToken()) {
        val cond = condExpr(THEN)
        newLinesOpt()
        val thenp = expr()
        val elsep = if (in.token == ELSE) { in.nextToken(); expr() }
                    else EmptyTree
        mkIf(cond, thenp, elsep)
      }

    /**    `match' { CaseClauses }
     */
    def matchExpr(t: Tree, start: Offset, mkMatch: (Tree, List[CaseDef]) => Match) =
      atSpan(start, in.skipToken()) {
        inBraces(mkMatch(t, caseClauses(caseClause)))
      }

    /**    `match' { ImplicitCaseClauses }
     */
    def impliedMatch(start: Int, imods: Modifiers) = {
      def markFirstIllegal(mods: List[Mod]) = mods match {
        case mod :: _ => syntaxError(em"illegal modifier for delegate match", mod.span)
        case _ =>
      }
      imods.mods match {
        case (Mod.Implicit() | Mod.Implied()) :: mods => markFirstIllegal(mods)
        case mods => markFirstIllegal(mods)
      }
      val result @ Match(t, cases) =
        matchExpr(EmptyTree, start, InlineMatch)
      for (CaseDef(pat, _, _) <- cases) {
        def isImplicitPattern(pat: Tree) = pat match {
          case Typed(pat1, _) => isVarPattern(pat1)
          case pat => isVarPattern(pat)
        }
        if (!isImplicitPattern(pat))
          syntaxError(em"not a legal pattern for a delegate match", pat.span)
      }
      result
    }

    /**    `match' { TypeCaseClauses }
     */
    def matchType(t: Tree): MatchTypeTree =
      atSpan(t.span.start, accept(MATCH)) {
        inBraces(MatchTypeTree(EmptyTree, t, caseClauses(typeCaseClause)))
      }

    /** FunParams         ::=  Bindings
     *                     |   id
     *                     |   `_'
     *  Bindings          ::=  `(' [Binding {`,' Binding}] `)'
     */
    def funParams(mods: Modifiers, location: Location.Value): List[Tree] =
      if (in.token == LPAREN)
        inParens(if (in.token == RPAREN) Nil else commaSeparated(() => binding(mods)))
      else {
        val start = in.offset
        val name = bindingName()
        val t =
          if (in.token == COLON && location == Location.InBlock) {
            if (ctx.settings.strict.value)
                // Don't error in non-strict mode, as the alternative syntax "implicit (x: T) => ... "
                // is not supported by Scala2.x
              migrationWarningOrError(s"This syntax is no longer supported; parameter needs to be enclosed in (...)")
            in.nextToken()
            val t = infixType()
            if (false && in.isScala2Mode) {
              patch(source, Span(start), "(")
              patch(source, Span(in.lastOffset), ")")
            }
            t
          }
          else TypeTree()
        (atSpan(start) { makeParameter(name, t, mods) }) :: Nil
      }

    /**  Binding           ::= (id | `_') [`:' Type]
     */
    def binding(mods: Modifiers): Tree =
      atSpan(in.offset) { makeParameter(bindingName(), typedOpt(), mods) }

    def bindingName(): TermName =
      if (in.token == USCORE) {
        in.nextToken()
        WildcardParamName.fresh()
      }
      else ident()

    /** Expr         ::= ClosureMods FunParams `=>' Expr
     *  BlockResult  ::= implicit id [`:' InfixType] `=>' Block // Scala2 only
     */
    def implicitClosure(start: Int, location: Location.Value, implicitMods: Modifiers): Tree =
      closureRest(start, location, funParams(implicitMods, location))

    def closureRest(start: Int, location: Location.Value, params: List[Tree]): Tree =
      atSpan(start, in.offset) {
        accept(ARROW)
        Function(params, if (location == Location.InBlock) block() else expr())
      }

    /** PostfixExpr   ::= InfixExpr [id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr id [nl] InfixExpr
     *                  | InfixExpr ‘given’ (InfixExpr | ParArgumentExprs)
     */
    def postfixExpr(): Tree =
      infixOps(prefixExpr(), canStartExpressionTokens, prefixExpr, maybePostfix = true)

    /** PrefixExpr   ::= [`-' | `+' | `~' | `!'] SimpleExpr
    */
    val prefixExpr: () => Tree = () =>
      if (isIdent && nme.raw.isUnary(in.name)) {
        val start = in.offset
        val op = termIdent()
        if (op.name == nme.raw.MINUS && isNumericLit)
          simpleExprRest(literal(start), canApply = true)
        else
          atSpan(start) { PrefixOp(op, simpleExpr()) }
      }
      else simpleExpr()

    /** SimpleExpr    ::= ‘new’ (ConstrApp [TemplateBody] | TemplateBody)
     *                 |  BlockExpr
     *                 |  ‘$’ ‘{’ Block ‘}’
     *                 |  Quoted
     *                 |  quoteId
     *                 |  SimpleExpr1 [`_']
     *  SimpleExpr1   ::= literal
     *                 |  xmlLiteral
     *                 |  Path
     *                 |  `(' [ExprsInParens] `)'
     *                 |  SimpleExpr `.' id
     *                 |  SimpleExpr (TypeArgs | NamedTypeArgs)
     *                 |  SimpleExpr1 ArgumentExprs
     *  Quoted        ::= ‘'’ ‘{’ Block ‘}’
     *                 |  ‘'’ ‘[’ Type ‘]’
     */
    def simpleExpr(): Tree = {
      var canApply = true
      val t = in.token match {
        case XMLSTART =>
          xmlLiteral()
        case IDENTIFIER =>
          if (isSplice) splice(isType = false)
          else path(thisOK = true)
        case BACKQUOTED_IDENT | THIS | SUPER =>
          path(thisOK = true)
        case USCORE =>
          val start = in.skipToken()
          val pname = WildcardParamName.fresh()
          val param = ValDef(pname, TypeTree(), EmptyTree).withFlags(SyntheticTermParam)
            .withSpan(Span(start))
          placeholderParams = param :: placeholderParams
          atSpan(start) { Ident(pname) }
        case LPAREN =>
          atSpan(in.offset) { makeTupleOrParens(inParens(exprsInParensOpt())) }
        case LBRACE =>
          canApply = false
          blockExpr()
        case QUOTE =>
          atSpan(in.skipToken()) {
            withinStaged(StageKind.Quoted) {
              Quote {
                if (in.token == LBRACKET) inBrackets(typ())
                else stagedBlock()
              }
            }
          }
        case NEW =>
          canApply = false
          newExpr()
        case MACRO =>
          val start = in.skipToken()
          val call = ident()
          // The standard library uses "macro ???" to denote "fast track" macros
          // hardcoded in the compiler, don't issue an error for those macros
          // since we want to be able to compile the standard library.
          if (call `ne` nme.???)
            syntaxError(
              "Scala 2 macros are not supported, see http://dotty.epfl.ch/docs/reference/dropped-features/macros.html",
              start)
          unimplementedExpr
        case _ =>
          if (isLiteral) literal()
          else {
            syntaxErrorOrIncomplete(IllegalStartSimpleExpr(tokenString(in.token)), expectedOffset)
            errorTermTree
          }
      }
      simpleExprRest(t, canApply)
    }

    def simpleExprRest(t: Tree, canApply: Boolean = true): Tree = {
      if (canApply) newLineOptWhenFollowedBy(LBRACE)
      in.token match {
        case DOT =>
          in.nextToken()
          simpleExprRest(selector(t), canApply = true)
        case LBRACKET =>
          val tapp = atSpan(startOffset(t), in.offset) { TypeApply(t, typeArgs(namedOK = true, wildOK = false)) }
          simpleExprRest(tapp, canApply = true)
        case LPAREN | LBRACE if canApply =>
          val app = atSpan(startOffset(t), in.offset) { Apply(t, argumentExprs()) }
          simpleExprRest(app, canApply = true)
        case USCORE =>
          atSpan(startOffset(t), in.skipToken()) { PostfixOp(t, Ident(nme.WILDCARD)) }
        case _ =>
          t
      }
    }

    /** SimpleExpr    ::= ‘new’ (ConstrApp {`with` ConstrApp} [TemplateBody] | TemplateBody)
     */
    def newExpr(): Tree = {
      val start = in.skipToken()
      def reposition(t: Tree) = t.withSpan(Span(start, in.lastOffset))
      newLineOptWhenFollowedBy(LBRACE)
      val parents =
        if (in.token == LBRACE) Nil
        else constrApp() :: {
          if (in.token == WITH) {
            // Enable this for 3.1, when we drop `with` for inheritance:
            //   in.errorUnlessInScala2Mode(
            //     "anonymous class with multiple parents is no longer supported; use a named class instead")
            in.nextToken()
            tokenSeparated(WITH, constrApp)
          }
          else Nil
        }
      newLineOptWhenFollowedBy(LBRACE)
      parents match {
        case parent :: Nil if in.token != LBRACE =>
          reposition(if (parent.isType) ensureApplied(wrapNew(parent)) else parent)
        case _ =>
          New(reposition(templateBodyOpt(emptyConstructor, parents, Nil)))
      }
    }

    /**   ExprsInParens     ::=  ExprInParens {`,' ExprInParens}
     */
    def exprsInParensOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else commaSeparated(exprInParens)

    /** ParArgumentExprs ::= `(' [ExprsInParens] `)'
     *                    |  `(' [ExprsInParens `,'] PostfixExpr `:' `_' `*' ')'
     */
    def parArgumentExprs(): List[Tree] =
      inParens(if (in.token == RPAREN) Nil else commaSeparated(argumentExpr))

    /** ArgumentExprs ::= ParArgumentExprs
     *                 |  [nl] BlockExpr
     */
    def argumentExprs(): List[Tree] =
      if (in.token == LBRACE) blockExpr() :: Nil else parArgumentExprs()

    val argumentExpr: () => Tree = () => exprInParens() match {
      case arg @ Assign(Ident(id), rhs) => cpy.NamedArg(arg)(id, rhs)
      case arg => arg
    }


    /** ArgumentExprss ::= {ArgumentExprs}
     */
    def argumentExprss(fn: Tree): Tree = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LPAREN || in.token == LBRACE) argumentExprss(Apply(fn, argumentExprs()))
      else fn
    }

    /** ParArgumentExprss ::= {ParArgumentExprs}
     *
     *  Special treatment for arguments to primary constructor annotations.
     *  (...) is considered an argument only if it does not look like a formal
     *  parameter list, i.e. does not start with `( <annot>* <mod>* ident : `
     *  Furthermore, `()` is considered a annotation argument only if it comes first.
     */
    def parArgumentExprss(fn: Tree): Tree = {
      def isLegalAnnotArg: Boolean = {
        val lookahead = in.lookaheadScanner
        (lookahead.token == LPAREN) && {
          lookahead.nextToken()
          if (lookahead.token == RPAREN)
            !fn.isInstanceOf[Trees.Apply[_]] // allow one () as annotation argument
          else if (lookahead.token == IDENTIFIER) {
            lookahead.nextToken()
            lookahead.token != COLON
          }
          else canStartExpressionTokens.contains(lookahead.token)
        }
      }
      if (in.token == LPAREN && (!inClassConstrAnnots || isLegalAnnotArg))
        parArgumentExprss(
          atSpan(startOffset(fn)) { Apply(fn, parArgumentExprs()) }
        )
      else fn
    }

    /** BlockExpr         ::= `{' BlockExprContents `}'
     *  BlockExprContents ::= CaseClauses | Block
     */
    def blockExpr(): Tree = atSpan(in.offset) {
      inDefScopeBraces {
        if (in.token == CASE) Match(EmptyTree, caseClauses(caseClause))
        else block()
      }
    }

    /** Block ::= BlockStatSeq
     *  @note  Return tree does not have a defined span.
     */
    def block(): Tree = {
      val stats = blockStatSeq()
      def isExpr(stat: Tree) = !(stat.isDef || stat.isInstanceOf[Import])
      if (stats.nonEmpty && isExpr(stats.last)) Block(stats.init, stats.last)
      else Block(stats, EmptyTree)
    }

    /** Guard ::= if PostfixExpr
     */
    def guard(): Tree =
      if (in.token == IF) { in.nextToken(); postfixExpr() }
      else EmptyTree

    /** Enumerators ::= Generator {semi Enumerator | Guard}
     */
    def enumerators(): List[Tree] = generator() :: enumeratorsRest()

    def enumeratorsRest(): List[Tree] =
      if (isStatSep) { in.nextToken(); enumerator() :: enumeratorsRest() }
      else if (in.token == IF) guard() :: enumeratorsRest()
      else Nil

    /** Enumerator  ::=  Generator
     *                |  Guard
     *                |  Pattern1 `=' Expr
     */
    def enumerator(): Tree =
      if (in.token == IF) guard()
      else if (in.token == CASE) generator()
      else {
        val pat = pattern1()
        if (in.token == EQUALS) atSpan(startOffset(pat), in.skipToken()) { GenAlias(pat, expr()) }
        else generatorRest(pat, casePat = false)
      }

    /** Generator   ::=  [‘case’] Pattern `<-' Expr
     */
    def generator(): Tree = {
      val casePat = if (in.token == CASE) { in.skipCASE(); true } else false
      generatorRest(pattern1(), casePat)
    }

    def generatorRest(pat: Tree, casePat: Boolean): GenFrom =
      atSpan(startOffset(pat), accept(LARROW)) {
        val checkMode =
          if (casePat) GenCheckMode.FilterAlways
          else if (ctx.settings.strict.value) GenCheckMode.Check
          else GenCheckMode.FilterNow  // filter for now, to keep backwards compat
        GenFrom(pat, expr(), checkMode)
      }

    /** ForExpr  ::= `for' (`(' Enumerators `)' | `{' Enumerators `}')
     *                {nl} [`yield'] Expr
     *            |  `for' Enumerators (`do' Expr | `yield' Expr)
     */
    def forExpr(): Tree = atSpan(in.skipToken()) {
      var wrappedEnums = true
      val enums =
        if (in.token == LBRACE) inBraces(enumerators())
        else if (in.token == LPAREN) {
          val lparenOffset = in.skipToken()
          openParens.change(LPAREN, 1)
          val res =
            if (in.token == CASE) enumerators()
            else {
              val pats = patternsOpt()
              val pat =
                if (in.token == RPAREN || pats.length > 1) {
                  wrappedEnums = false
                  accept(RPAREN)
                  openParens.change(LPAREN, -1)
                  atSpan(lparenOffset) { makeTupleOrParens(pats) } // note: alternatives `|' need to be weeded out by typer.
                }
                else pats.head
              generatorRest(pat, casePat = false) :: enumeratorsRest()
            }
          if (wrappedEnums) {
            accept(RPAREN)
            openParens.change(LPAREN, -1)
          }
          res
        } else {
          wrappedEnums = false
          enumerators()
        }
      newLinesOpt()
      if (in.token == YIELD) { in.nextToken(); ForYield(enums, expr()) }
      else if (in.token == DO) { in.nextToken(); ForDo(enums, expr()) }
      else {
        if (!wrappedEnums) syntaxErrorOrIncomplete(YieldOrDoExpectedInForComprehension())
        ForDo(enums, expr())
      }
    }

    /** CaseClauses         ::= CaseClause {CaseClause}
     *  ImplicitCaseClauses ::= ImplicitCaseClause {ImplicitCaseClause}
     *  TypeCaseClauses     ::= TypeCaseClause {TypeCaseClause}
     */
    def caseClauses(clause: () => CaseDef): List[CaseDef] = {
      val buf = new ListBuffer[CaseDef]
      buf += clause()
      while (in.token == CASE) buf += clause()
      buf.toList
    }

   /** CaseClause         ::= ‘case’ Pattern [Guard] `=>' Block
    *  ImplicitCaseClause ::= ‘case’ PatVar [Ascription] [Guard] `=>' Block
    */
    val caseClause: () => CaseDef = () => atSpan(in.offset) {
      accept(CASE)
      CaseDef(pattern(), guard(), atSpan(accept(ARROW)) { block() })
    }

    /** TypeCaseClause     ::= ‘case’ InfixType ‘=>’ Type [nl]
     */
    val typeCaseClause: () => CaseDef = () => atSpan(in.offset) {
      accept(CASE)
      CaseDef(infixType(), EmptyTree, atSpan(accept(ARROW)) {
        val t = typ()
        if (isStatSep) in.nextToken()
        t
      })
    }

    /* -------- PATTERNS ------------------------------------------- */

    /**  Pattern           ::=  Pattern1 { `|' Pattern1 }
     */
    val pattern: () => Tree = () => {
      val pat = pattern1()
      if (isIdent(nme.raw.BAR))
        atSpan(startOffset(pat)) { Alternative(pat :: patternAlts()) }
      else pat
    }

    def patternAlts(): List[Tree] =
      if (isIdent(nme.raw.BAR)) { in.nextToken(); pattern1() :: patternAlts() }
      else Nil

    /**  Pattern1          ::= PatVar Ascription
     *                      |  Pattern2
     */
    def pattern1(): Tree = {
      val p = pattern2()
      if (isVarPattern(p) && in.token == COLON) {
        in.nextToken()
        ascription(p, Location.InPattern)
      }
      else p
    }

    /**  Pattern2    ::=  [id `@'] InfixPattern
     */
    val pattern2: () => Tree = () => infixPattern() match {
      case p @ Ident(name) if in.token == AT =>
        val offset = in.skipToken()

        // compatibility for Scala2 `x @ _*` syntax
        infixPattern() match {
          case pt @ Ident(tpnme.WILDCARD_STAR) =>
            if (ctx.settings.strict.value)
              migrationWarningOrError("The syntax `x @ _*' is no longer supported; use `x : _*' instead", startOffset(p))
            atSpan(startOffset(p), offset) { Typed(p, pt) }
          case pt =>
            atSpan(startOffset(p), 0) { Bind(name, pt) }
        }
      case p @ Ident(tpnme.WILDCARD_STAR) =>
        // compatibility for Scala2 `_*` syntax
        if (ctx.settings.strict.value)
          migrationWarningOrError("The syntax `_*' is no longer supported; use `x : _*' instead", startOffset(p))
        atSpan(startOffset(p)) { Typed(Ident(nme.WILDCARD), p) }
      case p =>
        p
    }

    /**  InfixPattern ::= SimplePattern {id [nl] SimplePattern}
     */
    def infixPattern(): Tree =
      infixOps(simplePattern(), canStartExpressionTokens, simplePattern, isOperator = in.name != nme.raw.BAR)

    /** SimplePattern    ::= PatVar
     *                    |  Literal
     *                    |  Quoted
     *                    |  XmlPattern
     *                    |  `(' [Patterns] `)'
     *                    |  SimplePattern1 [TypeArgs] [ArgumentPatterns]
     *  SimplePattern1   ::= Path
     *                    |  SimplePattern1 `.' id
     *  PatVar           ::= id
     *                    |  `_'
     */
    val simplePattern: () => Tree = () => in.token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
        path(thisOK = true) match {
          case id @ Ident(nme.raw.MINUS) if isNumericLit => literal(startOffset(id))
          case t => simplePatternRest(t)
        }
      case USCORE =>
        val wildIndent = wildcardIdent()

        // compatibility for Scala2 `x @ _*` and `_*` syntax
        // `x: _*' is parsed in `ascription'
        if (isIdent(nme.raw.STAR)) {
          in.nextToken()
          if (in.token != RPAREN) syntaxError(SeqWildcardPatternPos(), wildIndent.span)
          atSpan(wildIndent.span) { Ident(tpnme.WILDCARD_STAR) }
        } else wildIndent
      case LPAREN =>
        atSpan(in.offset) { makeTupleOrParens(inParens(patternsOpt())) }
      case QUOTE =>
        simpleExpr()
      case XMLSTART =>
        xmlLiteralPattern()
      case _ =>
        if (isLiteral) literal(inPattern = true)
        else {
          syntaxErrorOrIncomplete(IllegalStartOfSimplePattern(), expectedOffset)
          errorTermTree
        }
    }

    def simplePatternRest(t: Tree): Tree = {
      var p = t
      if (in.token == LBRACKET)
        p = atSpan(startOffset(t), in.offset) { TypeApply(p, typeArgs(namedOK = false, wildOK = false)) }
      if (in.token == LPAREN)
        p = atSpan(startOffset(t), in.offset) { Apply(p, argumentPatterns()) }
      p
    }

    /** Patterns          ::=  Pattern [`,' Pattern]
     */
    def patterns(): List[Tree] = commaSeparated(pattern)

    def patternsOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else patterns()


    /** ArgumentPatterns  ::=  ‘(’ [Patterns] ‘)’
     *                      |  ‘(’ [Patterns ‘,’] Pattern2 ‘:’ ‘_’ ‘*’ ‘)’
     */
    def argumentPatterns(): List[Tree] =
      inParens(patternsOpt())

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    private def modOfToken(tok: Int, name: Name): Mod = tok match {
      case ABSTRACT    => Mod.Abstract()
      case FINAL       => Mod.Final()
      case IMPLICIT    => Mod.Implicit()
      case GIVEN       => Mod.Given()
      case ERASED      => Mod.Erased()
      case LAZY        => Mod.Lazy()
      case OVERRIDE    => Mod.Override()
      case PRIVATE     => Mod.Private()
      case PROTECTED   => Mod.Protected()
      case SEALED      => Mod.Sealed()
      case IDENTIFIER =>
        name match {
          case nme.inline => Mod.Inline()
          case nme.opaque => Mod.Opaque()
        }
    }

    /** Drop `private' modifier when followed by a qualifier.
     *  Contract `abstract' and `override' to ABSOVERRIDE
     */
    private def normalize(mods: Modifiers): Modifiers =
      if ((mods is Private) && mods.hasPrivateWithin)
        normalize(mods &~ Private)
      else if (mods is AbstractAndOverride)
        normalize(addFlag(mods &~ (Abstract | Override), AbsOverride))
      else
        mods

    private def addModifier(mods: Modifiers): Modifiers = {
      val tok = in.token
      val name = in.name
      val mod = atSpan(in.skipToken()) { modOfToken(tok, name) }

      if (mods is mod.flags) syntaxError(RepeatedModifier(mod.flags.toString))
      addMod(mods, mod)
    }

    private def compatible(flags1: FlagSet, flags2: FlagSet): Boolean = (
         flags1.isEmpty
      || flags2.isEmpty
      || flags1.isTermFlags && flags2.isTermFlags
      || flags1.isTypeFlags && flags2.isTypeFlags
    )

    def addFlag(mods: Modifiers, flag: FlagSet): Modifiers = {
      def getPrintableTypeFromFlagSet =
        Map(Trait -> "trait", Method -> "method", Mutable -> "variable").get(flag)

      if (compatible(mods.flags, flag)) mods | flag
      else {
        syntaxError(ModifiersNotAllowed(mods.flags, getPrintableTypeFromFlagSet))
        Modifiers(flag)
      }
    }

    /** Always add the syntactic `mod`, but check and conditionally add semantic `mod.flags`
     */
    def addMod(mods: Modifiers, mod: Mod): Modifiers =
      addFlag(mods, mod.flags).withAddedMod(mod)

    /** AccessQualifier ::= "[" (id | this) "]"
     */
    def accessQualifierOpt(mods: Modifiers): Modifiers =
      if (in.token == LBRACKET) {
        if ((mods is Local) || mods.hasPrivateWithin)
          syntaxError(DuplicatePrivateProtectedQualifier())
        inBrackets {
          if (in.token == THIS) { in.nextToken(); mods | Local }
          else mods.withPrivateWithin(ident().toTypeName)
        }
      } else mods

    /** {Annotation} {Modifier}
     *  Modifiers      ::= {Modifier}
     *  LocalModifiers ::= {LocalModifier}
     *  AccessModifier ::= (private | protected) [AccessQualifier]
     *  Modifier       ::= LocalModifier
     *                  |  AccessModifier
     *                  |  override
     *                  |  opaque
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy | erased | inline
     */
    def modifiers(allowed: BitSet = modifierTokens, start: Modifiers = Modifiers()): Modifiers = {
      @tailrec
      def loop(mods: Modifiers): Modifiers = {
        if (allowed.contains(in.token) ||
            in.isSoftModifier &&
              localModifierTokens.subsetOf(allowed)) { // soft modifiers are admissible everywhere local modifiers are
          val isAccessMod = accessModifierTokens contains in.token
          val mods1 = addModifier(mods)
          loop(if (isAccessMod) accessQualifierOpt(mods1) else mods1)
        } else if (in.token == NEWLINE && (mods.hasFlags || mods.hasAnnotations)) {
          in.nextToken()
          loop(mods)
        } else {
          mods
        }
      }
      normalize(loop(start))
    }

    /** ClosureMods ::=  { ‘implicit’ | ‘erased’ | ‘given’}
     *  FunTypeMods ::=  { ‘erased’ | ‘given’}
     */
    val closureMods: BitSet = BitSet(GIVEN, IMPLICIT, ERASED)
    val funTypeMods: BitSet = BitSet(GIVEN, ERASED)

    /** Wrap annotation or constructor in New(...).<init> */
    def wrapNew(tpt: Tree): Select = Select(New(tpt), nme.CONSTRUCTOR)

    /** Adjust start of annotation or constructor to offset of preceding @ or new */
    def adjustStart(start: Offset)(tree: Tree): Tree = {
      val tree1 = tree match {
        case Apply(fn, args) => cpy.Apply(tree)(adjustStart(start)(fn), args)
        case Select(qual, name) => cpy.Select(tree)(adjustStart(start)(qual), name)
        case _ => tree
      }
      if (tree1.span.exists && start < tree1.span.start)
        tree1.withSpan(tree1.span.withStart(start))
      else tree1
    }

    /** Annotation        ::=  `@' SimpleType {ParArgumentExprs}
     */
    def annot(): Tree =
      adjustStart(accept(AT)) {
        ensureApplied(parArgumentExprss(wrapNew(simpleType())))
      }

    def annotations(skipNewLines: Boolean = false): List[Tree] = {
      if (skipNewLines) newLineOptWhenFollowedBy(AT)
      if (in.token == AT) annot() :: annotations(skipNewLines)
      else Nil
    }

    def annotsAsMods(skipNewLines: Boolean = false): Modifiers =
      Modifiers() withAnnotations annotations(skipNewLines)

    def defAnnotsMods(allowed: BitSet): Modifiers =
      modifiers(allowed, annotsAsMods(skipNewLines = true))

 /* -------- PARAMETERS ------------------------------------------- */

    /** ClsTypeParamClause::=  ‘[’ ClsTypeParam {‘,’ ClsTypeParam} ‘]’
     *  ClsTypeParam      ::=  {Annotation} [‘+’ | ‘-’]
     *                         id [HkTypeParamClause] TypeParamBounds
     *
     *  DefTypeParamClause::=  ‘[’ DefTypeParam {‘,’ DefTypeParam} ‘]’
     *  DefTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeParamBounds
     *
     *  TypTypeParamCaluse::=  ‘[’ TypTypeParam {‘,’ TypTypeParam} ‘]’
     *  TypTypeParam      ::=  {Annotation} id [HkTypePamClause] TypeBounds
     *
     *  HkTypeParamClause ::=  ‘[’ HkTypeParam {‘,’ HkTypeParam} ‘]’
     *  HkTypeParam       ::=  {Annotation} [‘+’ | ‘-’] (id [HkTypePamClause] | ‘_’) TypeBounds
     */
    def typeParamClause(ownerKind: ParamOwner.Value): List[TypeDef] = inBrackets {
      def typeParam(): TypeDef = {
        val isAbstractOwner = ownerKind == ParamOwner.Type || ownerKind == ParamOwner.TypeParam
        val start = in.offset
        val mods =
          annotsAsMods() | {
            if (ownerKind == ParamOwner.Class) Param | PrivateLocal
            else Param
          } | {
            if (ownerKind == ParamOwner.Def) EmptyFlags
            else if (isIdent(nme.raw.PLUS)) { in.nextToken(); Covariant }
            else if (isIdent(nme.raw.MINUS)) { in.nextToken(); Contravariant }
            else EmptyFlags
          }
        atSpan(start, nameStart) {
          val name =
            if (isAbstractOwner && in.token == USCORE) {
              in.nextToken()
              WildcardParamName.fresh().toTypeName
            }
            else ident().toTypeName
          val hkparams = typeParamClauseOpt(ParamOwner.TypeParam)
          val bounds = if (isAbstractOwner) typeBounds() else typeParamBounds(name)
          TypeDef(name, lambdaAbstract(hkparams, bounds)).withMods(mods)
        }
      }
      commaSeparated(() => typeParam())
    }

    def typeParamClauseOpt(ownerKind: ParamOwner.Value): List[TypeDef] =
      if (in.token == LBRACKET) typeParamClause(ownerKind) else Nil

    /** ClsParamClause    ::=  [‘erased’] (‘(’ ClsParams ‘)’
     *  GivenClsParamClause::= ‘given’ [‘erased’] (‘(’ ClsParams ‘)’ | GivenTypes)
     *  ClsParams         ::=  ClsParam {‘,’ ClsParam}
     *  ClsParam          ::=  {Annotation}
     *
     *  DefParamClause    ::=  [‘erased’] (‘(’ DefParams ‘)’
     *  GivenParamClause  ::=  ‘given’ [‘erased’] (‘(’ DefParams ‘)’ | GivenTypes)
     *  DefParams         ::=  DefParam {‘,’ DefParam}
     *  DefParam          ::=  {Annotation} [‘inline’] Param
     *
     *  Param             ::=  id `:' ParamType [`=' Expr]
     *
     *  @return   the list of parameter definitions
     */
    def paramClause(ofClass: Boolean = false,                // owner is a class
                    ofCaseClass: Boolean = false,            // owner is a case class
                    prefix: Boolean = false,                 // clause precedes name of an extension method
                    firstClause: Boolean = false,            // clause is the first in regular list of clauses
                    initialMods: Modifiers = EmptyModifiers): List[ValDef] = {
      var impliedMods: Modifiers = initialMods

      def param(): ValDef = {
        val start = in.offset
        var mods = impliedMods.withAnnotations(annotations())
        if (ofClass) {
          mods = addFlag(modifiers(start = mods), ParamAccessor)
          mods =
            if (in.token == VAL) {
              in.nextToken()
              mods
            }
            else if (in.token == VAR) {
              val mod = atSpan(in.skipToken()) { Mod.Var() }
              addMod(mods, mod)
            }
            else {
              if (!(mods.flags &~ (ParamAccessor | Inline | impliedMods.flags)).isEmpty)
                syntaxError("`val' or `var' expected")
              if (firstClause && ofCaseClass) mods
              else mods | PrivateLocal
            }
        }
        else {
          if (isIdent(nme.inline) && in.isSoftModifierInParamModifierPosition)
            mods = addModifier(mods)
          mods |= Param
        }
        atSpan(start, nameStart) {
          val name = ident()
          accept(COLON)
          if (in.token == ARROW && ofClass && !(mods is Local))
            syntaxError(VarValParametersMayNotBeCallByName(name, mods is Mutable))
          val tpt = paramType()
          val default =
            if (in.token == EQUALS) { in.nextToken(); expr() }
            else EmptyTree
          if (impliedMods.mods.nonEmpty) {
            impliedMods = impliedMods.withMods(Nil) // keep only flags, so that parameter positions don't overlap
          }
          ValDef(name, tpt, default).withMods(mods)
        }
      }

      def checkVarArgsRules(vparams: List[ValDef]): Unit = vparams match {
        case Nil =>
        case _ :: Nil if !prefix =>
        case vparam :: rest =>
          vparam.tpt match {
            case PostfixOp(_, op) if op.name == tpnme.raw.STAR =>
              syntaxError(VarArgsParamMustComeLast(), vparam.tpt.span)
            case _ =>
          }
          checkVarArgsRules(rest)
      }

      // begin paramClause
      inParens {
        if (in.token == RPAREN && !prefix && !impliedMods.is(Given)) Nil
        else {
          if (in.token == IMPLICIT && !impliedMods.is(Given | Erased))
            impliedMods = addMod(impliedMods, atSpan(accept(IMPLICIT)) { Mod.Implicit() })
          val clause =
            if (prefix) param() :: Nil
            else commaSeparated(() => param())
          checkVarArgsRules(clause)
          clause
        }
      }
    }

    /** ClsParamClauses   ::=  {ClsParamClause} [[nl] ‘(’ [‘implicit’] ClsParams ‘)’]
     *                      |  {ClsParamClause} {GivenClsParamClause}
     *  DefParamClauses   ::=  {DefParamClause} [[nl] ‘(’ [‘implicit’] DefParams ‘)’]
     *                      |  {DefParamClause} {GivenParamClause}
     *
     *  @return  The parameter definitions
     */
    def paramClauses(ofClass: Boolean = false,
                     ofCaseClass: Boolean = false,
                     ofInstance: Boolean = false): List[List[ValDef]] = {
      def recur(firstClause: Boolean, nparams: Int, contextualOnly: Boolean): List[List[ValDef]] = {
        var initialMods = EmptyModifiers
        if (in.token == GIVEN) {
          in.nextToken()
          initialMods |= Given
        }
        if (in.token == ERASED) {
          in.nextToken()
          initialMods |= Erased
        }
        val isContextual = initialMods.is(Given)
        newLineOptWhenFollowedBy(LPAREN)
        def isParamClause: Boolean =
          !isContextual || {
            val lookahead = in.lookaheadScanner
            lookahead.nextToken()
            paramIntroTokens.contains(lookahead.token) && {
              lookahead.token != IDENTIFIER ||
              lookahead.name == nme.inline || {
                lookahead.nextToken()
                lookahead.token == COLON
              }
            }
          }
        if (in.token == LPAREN && isParamClause) {
          if (contextualOnly && !isContextual)
            if (ofInstance) syntaxError(em"parameters of instance definitions must come after `given'")
            else syntaxError(em"normal parameters cannot come after `given' clauses")
          val params = paramClause(
              ofClass = ofClass,
              ofCaseClass = ofCaseClass,
              firstClause = firstClause,
              initialMods = initialMods)
          val lastClause = params.nonEmpty && params.head.mods.flags.is(Implicit)
          params :: (if (lastClause) Nil else recur(firstClause = false, nparams + params.length, isContextual))
        }
        else if (isContextual) {
          val tps = commaSeparated(() => annotType())
          var counter = nparams
          def nextIdx = { counter += 1; counter }
          val paramFlags = if (ofClass) Private | Local | ParamAccessor else Param
          val params = tps.map(makeSyntheticParameter(nextIdx, _, paramFlags | Synthetic | Given))
          params :: recur(firstClause = false, nparams + params.length, isContextual)
        }
        else Nil
      }
      recur(firstClause = true, 0, ofInstance)
    }

/* -------- DEFS ------------------------------------------- */

    def finalizeDef(md: MemberDef, mods: Modifiers, start: Int): md.ThisTree[Untyped] =
      md.withMods(mods).setComment(in.getDocComment(start))

    type ImportConstr = (Boolean, Tree, List[Tree]) => Tree

    /** Import  ::= import [delegate] [ImportExpr {`,' ImportExpr}
     *  Export  ::= export [delegate] [ImportExpr {`,' ImportExpr}
     */
    def importClause(leading: Token, mkTree: ImportConstr): List[Tree] = {
      val offset = accept(leading)
      val importImplied = in.token == IMPLIED
      if (importImplied) in.nextToken()
      commaSeparated(importExpr(importImplied, mkTree)) match {
        case t :: rest =>
          // The first import should start at the start offset of the keyword.
          val firstPos =
            if (t.span.exists) t.span.withStart(offset)
            else Span(offset, in.lastOffset)
          t.withSpan(firstPos) :: rest
        case nil => nil
      }
    }

    /**  ImportExpr ::= StableId `.' (id | `_' | ImportSelectors)
     */
    def importExpr(importImplied: Boolean, mkTree: ImportConstr): () => Tree = {

      /** ImportSelectors ::= `{' {ImportSelector `,'} FinalSelector ‘}’
       *  FinalSelector   ::=  ImportSelector
       *                    |  ‘_’
       *                    |  ‘for’ InfixType {‘,’ InfixType}
       */
      def importSelectors(): List[Tree] = in.token match {
        case USCORE =>
          wildcardIdent() :: Nil
        case FOR =>
          if (!importImplied)
              syntaxError(em"`for` qualifier only allowed in `import delegate`")
          atSpan(in.skipToken()) {
            var t = infixType()
            while (in.token == COMMA) {
              val op = atSpan(in.skipToken()) { Ident(tpnme.raw.BAR) }
              t = InfixOp(t, op, infixType())
            }
            TypeBoundsTree(EmptyTree, t)
          } :: Nil
        case _ =>
          importSelector() :: {
            if (in.token == COMMA) {
              in.nextToken()
              importSelectors()
            }
            else Nil
          }
      }

      /** ImportSelector ::= id [`=>' id | `=>' `_']
       */
      def importSelector(): Tree = {
        val from = termIdent()
        if (in.token == ARROW)
          atSpan(startOffset(from), in.skipToken()) {
            val start = in.offset
            val to = if (in.token == USCORE) wildcardIdent() else termIdent()
            val toWithPos =
              if (to.name == nme.ERROR)
                // error identifiers don't consume any characters, so atSpan(start)(id) wouldn't set a span.
                // Some testcases would then fail in Positioned.checkPos. Set a span anyway!
                atSpan(start, start, in.lastOffset)(to)
              else
                to
            Thicket(from, toWithPos)
          }
        else from
      }

      val handleImport: Tree => Tree = { tree: Tree =>
        if (in.token == USCORE) mkTree(importImplied, tree, wildcardIdent() :: Nil)
        else if (in.token == LBRACE) mkTree(importImplied, tree, inBraces(importSelectors()))
        else tree
      }

      def derived(impExp: Tree, qual: Tree, selectors: List[Tree]) =
        mkTree(importImplied, qual, selectors).withSpan(impExp.span)

      () => {
        val p = path(thisOK = false, handleImport)
        p match {
          case _: Import | _: Export => p
          case sel @ Select(qual, name) =>
            val selector = atSpan(pointOffset(sel)) { Ident(name) }
            mkTree(importImplied, qual, selector :: Nil).withSpan(sel.span)
          case t =>
            accept(DOT)
            mkTree(importImplied, t, Ident(nme.WILDCARD) :: Nil)
        }
      }
    }


    def posMods(start: Int, mods: Modifiers): Modifiers = {
      in.nextToken()
      mods
    }

    /** Def      ::= val PatDef
     *             | var VarDef
     *             | def DefDef
     *             | type {nl} TypeDcl
     *             | TmplDef
     *  Dcl      ::= val ValDcl
     *             | var ValDcl
     *             | def DefDcl
     *             | type {nl} TypeDcl
     *  EnumCase ::= `case' (id ClassConstr [`extends' ConstrApps]] | ids)
     */
    def defOrDcl(start: Int, mods: Modifiers): Tree = in.token match {
      case VAL =>
        in.nextToken()
        patDefOrDcl(start, mods)
      case VAR =>
        val mod = atSpan(in.skipToken()) { Mod.Var() }
        val mod1 = addMod(mods, mod)
        patDefOrDcl(start, mod1)
      case DEF =>
        defDefOrDcl(start, posMods(start, mods))
      case TYPE =>
        typeDefOrDcl(start, posMods(start, mods))
      case CASE if inEnum =>
        enumCase(start, mods)
      case _ =>
        tmplDef(start, mods)
    }

    /** PatDef  ::=  ids [‘:’ Type] ‘=’ Expr
     *            |  Pattern2 [‘:’ Type | Ascription] ‘=’ Expr
     *  VarDef  ::=  PatDef | id {`,' id} `:' Type `=' `_'
     *  ValDcl  ::=  id {`,' id} `:' Type
     *  VarDcl  ::=  id {`,' id} `:' Type
     */
    def patDefOrDcl(start: Offset, mods: Modifiers): Tree = atSpan(start, nameStart) {
      val first = pattern2()
      var lhs = first match {
        case id: Ident if in.token == COMMA =>
          in.nextToken()
          id :: commaSeparated(() => termIdent())
        case _ =>
          first :: Nil
      }
      def emptyType = TypeTree().withSpan(Span(in.lastOffset))
      val tpt =
        if (in.token == COLON) {
          in.nextToken()
          if (in.token == AT && lhs.tail.isEmpty) {
            lhs = ascription(first, Location.ElseWhere) :: Nil
            emptyType
          }
          else toplevelTyp()
        }
        else emptyType
      val rhs =
        if (tpt.isEmpty || in.token == EQUALS) {
          accept(EQUALS)
          if (in.token == USCORE && !tpt.isEmpty && (mods is Mutable) &&
              (lhs.toList forall (_.isInstanceOf[Ident]))) {
            wildcardIdent()
          } else {
            expr()
          }
        } else EmptyTree
      lhs match {
        case (id: BackquotedIdent) :: Nil if id.name.isTermName =>
          finalizeDef(BackquotedValDef(id.name.asTermName, tpt, rhs), mods, start)
        case Ident(name: TermName) :: Nil =>
          finalizeDef(ValDef(name, tpt, rhs), mods, start)
        case _ =>
          PatDef(mods, lhs, tpt, rhs)
      }
    }

    /** DefDef ::= DefSig [(‘:’ | ‘<:’) Type] ‘=’ Expr
     *           | this ParamClause ParamClauses `=' ConstrExpr
     *  DefDcl ::= DefSig `:' Type
     *  DefSig ::= ‘(’ DefParam ‘)’ [nl] id [DefTypeParamClause] ParamClauses
     */
    def defDefOrDcl(start: Offset, mods: Modifiers): Tree = atSpan(start, nameStart) {
      def scala2ProcedureSyntax(resultTypeStr: String) = {
        val toInsert =
          if (in.token == LBRACE) s"$resultTypeStr ="
          else ": Unit "  // trailing space ensures that `def f()def g()` works.
        in.testScala2Mode(s"Procedure syntax no longer supported; `$toInsert' should be inserted here") && {
          patch(source, Span(in.lastOffset), toInsert)
          true
        }
      }
      if (in.token == THIS) {
        in.nextToken()
        val vparamss = paramClauses()
        if (vparamss.isEmpty || vparamss.head.take(1).exists(_.mods.is(ImplicitOrGiven)))
          in.token match {
            case LBRACKET   => syntaxError("no type parameters allowed here")
            case EOF        => incompleteInputError(AuxConstructorNeedsNonImplicitParameter())
            case _          => syntaxError(AuxConstructorNeedsNonImplicitParameter(), nameStart)
          }
        if (in.isScala2Mode) newLineOptWhenFollowedBy(LBRACE)
        val rhs = {
          if (!(in.token == LBRACE && scala2ProcedureSyntax(""))) accept(EQUALS)
          atSpan(in.offset) { constrExpr() }
        }
        makeConstructor(Nil, vparamss, rhs).withMods(mods).setComment(in.getDocComment(start))
      } else {
        val (leadingParamss, flags) =
          if (in.token == LPAREN)
            (paramClause(prefix = true) :: Nil, Method | Extension)
          else
            (Nil, Method)
        val mods1 = addFlag(mods, flags)
        val ident = termIdent()
        val tparams = typeParamClauseOpt(ParamOwner.Def)
        val vparamss = paramClauses() match {
          case rparams :: rparamss if leadingParamss.nonEmpty && !isLeftAssoc(ident.name) =>
            rparams :: leadingParamss ::: rparamss
          case rparamss =>
            leadingParamss ::: rparamss
        }
        var tpt = fromWithinReturnType {
          if (in.token == SUBTYPE && mods.is(Inline)) {
            in.nextToken()
            TypeBoundsTree(EmptyTree, toplevelTyp())
          }
          else typedOpt()
        }
        if (in.isScala2Mode) newLineOptWhenFollowedBy(LBRACE)
        val rhs =
          if (in.token == EQUALS) {
            in.nextToken()
            expr()
          }
          else if (!tpt.isEmpty)
            EmptyTree
          else if (scala2ProcedureSyntax(": Unit")) {
            tpt = scalaUnit
            if (in.token == LBRACE) expr()
            else EmptyTree
          }
          else {
            if (!isExprIntro) syntaxError(MissingReturnType(), in.lastOffset)
            accept(EQUALS)
            expr()
          }

        if (ident.isBackquoted) finalizeDef(BackquotedDefDef(ident.name.asTermName, tparams, vparamss, tpt, rhs), mods1, start)
        else finalizeDef(DefDef(ident.name.asTermName, tparams, vparamss, tpt, rhs), mods1, start)
      }
    }

    /** ConstrExpr      ::=  SelfInvocation
     *                    |  ConstrBlock
     */
    def constrExpr(): Tree =
      if (in.token == LBRACE) constrBlock()
      else Block(selfInvocation() :: Nil, Literal(Constant(())))

    /** SelfInvocation  ::= this ArgumentExprs {ArgumentExprs}
     */
    def selfInvocation(): Tree =
      atSpan(accept(THIS)) {
        newLineOptWhenFollowedBy(LBRACE)
        argumentExprss(Apply(Ident(nme.CONSTRUCTOR), argumentExprs()))
      }

    /** ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     */
    def constrBlock(): Tree =
      atSpan(in.skipToken()) {
        val stats = selfInvocation() :: {
          if (isStatSep) { in.nextToken(); blockStatSeq() }
          else Nil
        }
        accept(RBRACE)
        Block(stats, Literal(Constant(())))
      }

    /** TypeDcl ::=  id [TypeParamClause] TypeBounds [‘=’ Type]
     */
    def typeDefOrDcl(start: Offset, mods: Modifiers): Tree = {
      newLinesOpt()
      atSpan(start, nameStart) {
        val name = ident().toTypeName
        val tparams = typeParamClauseOpt(ParamOwner.Type)
        def makeTypeDef(rhs: Tree): Tree =
          finalizeDef(TypeDef(name, lambdaAbstract(tparams, rhs)), mods, start)
        in.token match {
          case EQUALS =>
            in.nextToken()
            makeTypeDef(toplevelTyp())
          case SUBTYPE | SUPERTYPE =>
            val bounds = typeBounds()
            if (in.token == EQUALS) {
              val eqOffset = in.skipToken()
              var rhs = toplevelTyp()
              rhs match {
                case mtt: MatchTypeTree =>
                  bounds match {
                    case TypeBoundsTree(EmptyTree, upper) =>
                      rhs = MatchTypeTree(upper, mtt.selector, mtt.cases)
                    case _ =>
                      syntaxError(i"cannot combine lower bound and match type alias", eqOffset)
                  }
                case _ =>
                  if (mods.is(Opaque)) {
                    val annotType = AppliedTypeTree(
                      TypeTree(defn.WithBoundsAnnotType),
                        bounds.lo.orElse(TypeTree(defn.NothingType)) ::
                        bounds.hi.orElse(TypeTree(defn.AnyType)) :: Nil)
                    rhs = Annotated(rhs, ensureApplied(wrapNew(annotType)))
                  }
                  else syntaxError(i"cannot combine bound and alias", eqOffset)
              }
              makeTypeDef(rhs)
            }
            else makeTypeDef(bounds)
          case SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF =>
            makeTypeDef(typeBounds())
          case _ =>
            syntaxErrorOrIncomplete(ExpectedTypeBoundOrEquals(in.token))
            return EmptyTree // return to avoid setting the span to EmptyTree
        }
      }
    }

    /** TmplDef ::=  ([‘case’] ‘class’ | ‘trait’) ClassDef
     *            |  [‘case’] ‘object’ ObjectDef
     *            |  ‘enum’ EnumDef
     *            |  ‘instance’ InstanceDef
     */
    def tmplDef(start: Int, mods: Modifiers): Tree = {
      in.token match {
        case TRAIT =>
          classDef(start, posMods(start, addFlag(mods, Trait)))
        case CLASS =>
          classDef(start, posMods(start, mods))
        case CASECLASS =>
          classDef(start, posMods(start, mods | Case))
        case OBJECT =>
          objectDef(start, posMods(start, mods | Module))
        case CASEOBJECT =>
          objectDef(start, posMods(start, mods | Case | Module))
        case ENUM =>
          enumDef(start, posMods(start, mods | Enum))
        case IMPLIED =>
          instanceDef(start, mods, atSpan(in.skipToken()) { Mod.Implied() })
        case _ =>
          syntaxErrorOrIncomplete(ExpectedStartOfTopLevelDefinition())
          EmptyTree
      }
    }

    /** ClassDef ::= id ClassConstr TemplateOpt
     */
    def classDef(start: Offset, mods: Modifiers): TypeDef = atSpan(start, nameStart) {
      classDefRest(start, mods, ident().toTypeName)
    }

    def classDefRest(start: Offset, mods: Modifiers, name: TypeName): TypeDef = {
      val constr = classConstr(isCaseClass = mods.is(Case))
      val templ = templateOpt(constr)
      finalizeDef(TypeDef(name, templ), mods, start)
    }

    /** ClassConstr ::= [ClsTypeParamClause] [ConstrMods] ClsParamClauses
     */
    def classConstr(isCaseClass: Boolean = false): DefDef = atSpan(in.lastOffset) {
      val tparams = typeParamClauseOpt(ParamOwner.Class)
      val cmods = fromWithinClassConstr(constrModsOpt())
      val vparamss = paramClauses(ofClass = true, ofCaseClass = isCaseClass)
      makeConstructor(tparams, vparamss).withMods(cmods)
    }

    /** ConstrMods        ::=  {Annotation} [AccessModifier]
     */
    def constrModsOpt(): Modifiers =
      modifiers(accessModifierTokens, annotsAsMods())

    /** ObjectDef       ::= id TemplateOpt
     */
    def objectDef(start: Offset, mods: Modifiers): ModuleDef = atSpan(start, nameStart) {
      objectDefRest(start, mods, ident())
    }

    def objectDefRest(start: Offset, mods: Modifiers, name: TermName): ModuleDef = {
      val templ = templateOpt(emptyConstructor)
      finalizeDef(ModuleDef(name, templ), mods, start)
    }

    /**  EnumDef ::=  id ClassConstr InheritClauses EnumBody
     */
    def enumDef(start: Offset, mods: Modifiers): TypeDef = atSpan(start, nameStart) {
      val modName = ident()
      val clsName = modName.toTypeName
      val constr = classConstr()
      val templ = template(constr, isEnum = true)
      finalizeDef(TypeDef(clsName, templ), mods, start)
    }

    /** EnumCase = `case' (id ClassConstr [`extends' ConstrApps] | ids)
     */
    def enumCase(start: Offset, mods: Modifiers): DefTree = {
      val mods1 = mods | EnumCase
      in.skipCASE()

      atSpan(start, nameStart) {
        val id = termIdent()
        if (in.token == COMMA) {
          in.nextToken()
          val ids = commaSeparated(() => termIdent())
          PatDef(mods1, id :: ids, TypeTree(), EmptyTree)
        }
        else {
          val caseDef =
            if (in.token == LBRACKET || in.token == LPAREN || in.token == AT || isModifier) {
              val clsName = id.name.toTypeName
              val constr = classConstr(isCaseClass = true)
              TypeDef(clsName, caseTemplate(constr))
            }
            else
              ModuleDef(id.name.toTermName, caseTemplate(emptyConstructor))
          finalizeDef(caseDef, mods1, start)
        }
      }
    }

    /** [`extends' ConstrApps] */
    def caseTemplate(constr: DefDef): Template = {
      val parents =
        if (in.token == EXTENDS) {
          in.nextToken()
          tokenSeparated(WITH, constrApp)
        }
        else Nil
      Template(constr, parents, Nil, EmptyValDef, Nil)
    }

    /** InstanceDef    ::=  [id] [DefTypeParamClause] InstanceBody
     *  InstanceParams ::=  [DefTypeParamClause] {GivenParamClause}
     *  InstanceBody   ::=  [‘for’ ConstrApp {‘,’ ConstrApp }] {GivenParamClause} [TemplateBody]
     *                   |  ‘for’ Type {GivenParamClause} ‘=’ Expr
     */
    def instanceDef(start: Offset, mods: Modifiers, instanceMod: Mod) = atSpan(start, nameStart) {
      var mods1 = addMod(mods, instanceMod)
      val name = if (isIdent) ident() else EmptyTermName
      val tparams = typeParamClauseOpt(ParamOwner.Def)
      val parents =
        if (in.token == FOR) {
          in.nextToken()
          tokenSeparated(COMMA, constrApp)
        }
        else Nil
      val vparamss = paramClauses(ofInstance = true)
      val instDef =
        if (in.token == EQUALS && parents.length == 1 && parents.head.isType) {
          in.nextToken()
          mods1 |= Final
          DefDef(name, tparams, vparamss, parents.head, expr())
        }
        else {
          newLineOptWhenFollowedBy(LBRACE)
          val tparams1 = tparams.map(tparam => tparam.withMods(tparam.mods | PrivateLocal))
          val vparamss1 = vparamss.map(_.map(vparam =>
            vparam.withMods(vparam.mods &~ Param | ParamAccessor | PrivateLocal)))
          val templ = templateBodyOpt(makeConstructor(tparams1, vparamss1), parents, Nil)
          if (tparams.isEmpty && vparamss.isEmpty) ModuleDef(name, templ)
          else TypeDef(name.toTypeName, templ)
        }
      finalizeDef(instDef, mods1, start)
    }

/* -------- TEMPLATES ------------------------------------------- */

    /** SimpleConstrApp  ::=  AnnotType {ParArgumentExprs}
     *  ConstrApp        ::=  SimpleConstrApp
     *                     |  ‘(’ SimpleConstrApp {‘given’ (PrefixExpr | ParArgumentExprs)} ‘)’
     */
    val constrApp: () => Tree = () => {

      def isAnnotType(t: Tree) = t match {
        case _: Ident
           | _: Select
           | _: AppliedTypeTree
           | _: Tuple
           | _: Parens
           | _: RefinedTypeTree
           | _: SingletonTypeTree
           | _: TypSplice
           | _: Annotated => true
        case _ => false
      }

      def givenArgs(t: Tree): Tree = {
        if (in.token == GIVEN) givenArgs(applyGiven(t, prefixExpr)) else t
      }

      if (in.token == LPAREN)
        inParens {
          val t = toplevelTyp()
          if (isAnnotType(t))
            if (in.token == LPAREN) givenArgs(parArgumentExprss(wrapNew(t)))
            else if (in.token == GIVEN) givenArgs(wrapNew(t))
            else t
          else Parens(t)
        }
      else {
        val t = rejectWildcardType(annotType(), fallbackTree = Ident(nme.ERROR))
          // Using Ident(nme.ERROR) to avoid causing cascade errors on non-user-written code
        if (in.token == LPAREN) parArgumentExprss(wrapNew(t)) else t
      }
    }

    /** ConstrApps ::=  ConstrApp {‘with’ ConstrApp}  (to be deprecated in 3.1)
     *               |  ConstrApp {‘,’ ConstrApp}
     */
    def constrApps(): List[Tree] = {
      val t = constrApp()
      val ts =
        if (in.token == WITH) {
          in.nextToken()
          tokenSeparated(WITH, constrApp)
        }
        else if (in.token == COMMA) {
          in.nextToken()
          tokenSeparated(COMMA, constrApp)
        }
        else Nil
      t :: ts
    }

    /** InheritClauses ::=  [‘extends’ ConstrApps] [‘derives’ QualId {‘,’ QualId}]
     */
    def inheritClauses(): (List[Tree], List[Tree]) = {
      val extended =
        if (in.token == EXTENDS) {
          in.nextToken()
          if (in.token == LBRACE) {
            in.errorOrMigrationWarning("`extends' must be followed by at least one parent")
            Nil
          }
          else constrApps()
        }
        else Nil
      val derived =
        if (isIdent(nme.derives)) {
          in.nextToken()
          tokenSeparated(COMMA, () => convertToTypeId(qualId()))
        }
        else Nil
      (extended, derived)
    }

    /** Template          ::=  InheritClauses [TemplateBody]
     */
    def template(constr: DefDef, isEnum: Boolean = false): Template = {
      val (parents, derived) = inheritClauses()
      newLineOptWhenFollowedBy(LBRACE)
      if (isEnum) {
        val (self, stats) = withinEnum(templateBody())
        Template(constr, parents, derived, self, stats)
      }
      else templateBodyOpt(constr, parents, derived)
    }

    /** TemplateOpt = [Template]
     */
    def templateOpt(constr: DefDef): Template = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == EXTENDS || isIdent(nme.derives) || in.token == LBRACE)
        template(constr)
      else
        Template(constr, Nil, Nil, EmptyValDef, Nil)
    }

    /** TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     */
    def templateBodyOpt(constr: DefDef, parents: List[Tree], derived: List[Tree]): Template = {
      val (self, stats) =
        if (in.token == LBRACE) templateBody() else (EmptyValDef, Nil)
      Template(constr, parents, derived, self, stats)
    }

    def templateBody(): (ValDef, List[Tree]) = {
      val r = inDefScopeBraces { templateStatSeq() }
      if (in.token == WITH) {
        syntaxError(EarlyDefinitionsNotSupported())
        in.nextToken()
        template(emptyConstructor)
      }
      r
    }

/* -------- STATSEQS ------------------------------------------- */

    /** Create a tree representing a packaging */
    def makePackaging(start: Int, pkg: Tree, stats: List[Tree]): PackageDef = pkg match {
      case x: RefTree => atSpan(start, pointOffset(pkg))(PackageDef(x, stats))
    }

    /** Packaging ::= package QualId [nl] `{' TopStatSeq `}'
     */
    def packaging(start: Int): Tree = {
      val pkg = qualId()
      newLineOptWhenFollowedBy(LBRACE)
      val stats = inDefScopeBraces(topStatSeq())
      makePackaging(start, pkg, stats)
    }

    /** TopStatSeq ::= TopStat {semi TopStat}
     *  TopStat ::= Import
     *            | Export
     *            | Annotations Modifiers Def
     *            | Packaging
     *            | package object objectDef
     *            |
     */
    def topStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        setLastStatOffset()
        if (in.token == PACKAGE) {
          val start = in.skipToken()
          if (in.token == OBJECT) {
            in.nextToken()
            stats += objectDef(start, Modifiers(Package))
          }
          else stats += packaging(start)
        }
        else if (in.token == IMPORT)
          stats ++= importClause(IMPORT, Import)
        else if (in.token == EXPORT)
          stats ++= importClause(EXPORT, Export.apply)
        else if (in.token == AT || isDefIntro(modifierTokens))
          stats +++= defOrDcl(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          if (in.token == CASE)
            syntaxErrorOrIncomplete(OnlyCaseClassOrCaseObjectAllowed())
          else
            syntaxErrorOrIncomplete(ExpectedToplevelDef())
        }
        acceptStatSepUnlessAtEnd()
      }
      stats.toList
    }

    /** TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStat {semi TemplateStat}
     *  TemplateStat     ::= Import
     *                     | Export
     *                     | Annotations Modifiers Def
     *                     | Annotations Modifiers Dcl
     *                     | Expr1
     *                     |
     *  EnumStat         ::= TemplateStat
     *                     | Annotations Modifiers EnumCase
     */
    def templateStatSeq(): (ValDef, List[Tree]) = checkNoEscapingPlaceholders {
      var self: ValDef = EmptyValDef
      val stats = new ListBuffer[Tree]
      if (isExprIntro) {
        val first = expr1()
        if (in.token == ARROW) {
          first match {
            case Typed(tree @ This(EmptyTypeIdent), tpt) =>
              self = makeSelfDef(nme.WILDCARD, tpt).withSpan(first.span)
            case _ =>
              val ValDef(name, tpt, _) = convertToParam(first, "self type clause")
              if (name != nme.ERROR)
                self = makeSelfDef(name, tpt).withSpan(first.span)
          }
          in.nextToken()
        } else {
          stats += first
          acceptStatSepUnlessAtEnd()
        }
      }
      var exitOnError = false
      while (!isStatSeqEnd && !exitOnError) {
        setLastStatOffset()
        if (in.token == IMPORT)
          stats ++= importClause(IMPORT, Import)
        else if (in.token == EXPORT)
          stats ++= importClause(EXPORT, Export.apply)
        else if (isExprIntro)
          stats += expr1()
        else if (isDefIntro(modifierTokensOrCase))
          stats +++= defOrDcl(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          exitOnError = mustStartStat
          syntaxErrorOrIncomplete("illegal start of definition")
        }
        acceptStatSepUnlessAtEnd()
      }
      (self, if (stats.isEmpty) List(EmptyTree) else stats.toList)
    }

    /** RefineStatSeq    ::=  RefineStat {semi RefineStat}
     *  RefineStat       ::=  ‘val’ VarDcl
     *                     |  ‘def’ DefDcl
     *                     |  ‘type’ {nl} TypeDcl
     *  (in reality we admit Defs and vars and filter them out afterwards in `checkLegal`)
     */
    def refineStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      def checkLegal(tree: Tree): List[Tree] = {
        val isLegal = tree match {
          case tree: ValDef => tree.rhs.isEmpty && !tree.mods.flags.is(Mutable)
          case tree: DefDef => tree.rhs.isEmpty
          case tree: TypeDef => true
          case _ => false
        }
        if (isLegal) tree :: Nil
        else {
          syntaxError("illegal refinement", tree.span)
          Nil
        }
      }
      while (!isStatSeqEnd) {
        if (isDclIntro)
          stats ++= checkLegal(defOrDcl(in.offset, Modifiers()))
        else if (!isStatSep)
          syntaxErrorOrIncomplete(
            "illegal start of declaration" +
            (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
             else ""))
        acceptStatSepUnlessAtEnd()
      }
      stats.toList
    }

    def localDef(start: Int, implicitMods: Modifiers = EmptyModifiers): Tree = {
      var mods = defAnnotsMods(localModifierTokens)
      for (imod <- implicitMods.mods) mods = addMod(mods, imod)
      if (mods.is(Final)) {
        // A final modifier means the local definition is "class-like".  // FIXME: Deal with modifiers separately
        tmplDef(start, mods)
      } else {
        defOrDcl(start, mods)
      }
    }

    /** BlockStatSeq ::= { BlockStat semi } [Expr]
     *  BlockStat    ::= Import
     *                 | Annotations [implicit] [lazy] Def
     *                 | Annotations LocalModifiers TmplDef
     *                 | Expr1
     *                 |
     */
    def blockStatSeq(): List[Tree] = checkNoEscapingPlaceholders {
      val stats = new ListBuffer[Tree]
      var exitOnError = false
      while (!isStatSeqEnd && in.token != CASE && !exitOnError) {
        setLastStatOffset()
        if (in.token == IMPORT)
          stats ++= importClause(IMPORT, Import)
        else if (in.token == GIVEN)
          stats += implicitClosure(in.offset, Location.InBlock, modifiers(closureMods))
        else if (isExprIntro)
          stats += expr(Location.InBlock)
        else if (isDefIntro(localModifierTokens, excludedSoftModifiers = Set(nme.`opaque`)))
          if (closureMods.contains(in.token)) {
            val start = in.offset
            var imods = modifiers(closureMods)
            if (isBindingIntro)
              stats += implicitClosure(start, Location.InBlock, imods)
            else if (in.token == MATCH)
              stats += impliedMatch(start, imods)
            else
              stats +++= localDef(start, imods)
          } else {
            stats +++= localDef(in.offset)
          }
        else if (!isStatSep && (in.token != CASE)) {
          exitOnError = mustStartStat
          syntaxErrorOrIncomplete(IllegalStartOfStatement(isModifier))
        }
        acceptStatSepUnlessAtEnd(CASE)
      }
      stats.toList
    }

    /** CompilationUnit ::= {package QualId semi} TopStatSeq
     */
    def compilationUnit(): Tree = checkNoEscapingPlaceholders {
      def topstats(): List[Tree] = {
        val ts = new ListBuffer[Tree]
        while (in.token == SEMI) in.nextToken()
        val start = in.offset
        if (in.token == PACKAGE) {
          in.nextToken()
          if (in.token == OBJECT) {
            in.nextToken()
            ts += objectDef(start, Modifiers(Package))
            if (in.token != EOF) {
              acceptStatSep()
              ts ++= topStatSeq()
            }
          } else {
            val pkg = qualId()
            newLineOptWhenFollowedBy(LBRACE)
            if (in.token == EOF)
              ts += makePackaging(start, pkg, List())
            else if (in.token == LBRACE) {
              ts += inDefScopeBraces(makePackaging(start, pkg, topStatSeq()))
              acceptStatSepUnlessAtEnd()
              ts ++= topStatSeq()
            }
            else {
              acceptStatSep()
              ts += makePackaging(start, pkg, topstats())
            }
          }
        }
        else
          ts ++= topStatSeq()

        ts.toList
      }

      topstats() match {
        case List(stat @ PackageDef(_, _)) => stat
        case Nil => EmptyTree  // without this case we'd get package defs without positions
        case stats => PackageDef(Ident(nme.EMPTY_PACKAGE), stats)
      }
    }
  }


  /** OutlineParser parses top-level declarations in `source` to find declared classes, ignoring their bodies (which
   *  must only have balanced braces). This is used to map class names to defining sources.
   */
  class OutlineParser(source: SourceFile)(implicit ctx: Context) extends Parser(source) with OutlineParserCommon {

    def skipBracesHook(): Option[Tree] =
      if (in.token == XMLSTART) Some(xmlLiteral()) else None

    override def blockExpr(): Tree = {
      skipBraces()
      EmptyTree
    }

    override def templateBody(): (ValDef, List[Thicket]) = {
      skipBraces()
      (EmptyValDef, List(EmptyTree))
    }
  }
}
