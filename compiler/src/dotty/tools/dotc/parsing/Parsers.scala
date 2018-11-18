package dotty.tools
package dotc
package parsing

import scala.annotation.internal.sharable
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.BitSet
import util.{ SourceFile, SourcePosition }
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
import util.Positions._
import Constants._
import ScriptParsers._
import Decorators._
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
     *  If `t` does not have a position yet, set its position to the given one.
     */
    def atPos[T <: Positioned](pos: Position)(t: T): T =
      if (t.pos.isSourceDerived) t else t.withPos(pos)

    def atPos[T <: Positioned](start: Offset, point: Offset, end: Offset)(t: T): T =
      atPos(Position(start, end, point))(t)

    /** If the last read offset is strictly greater than `start`, position tree
     *  to position spanning from `start` to last read offset, with given point.
     *  If the last offset is less than or equal to start, the tree `t` did not
     *  consume any source for its construction. In this case, don't position it yet,
     *  but wait for its position to be determined by `setChildPositions` when the
     *  parent node is positioned.
     */
    def atPos[T <: Positioned](start: Offset, point: Offset)(t: T): T =
      if (in.lastOffset > start) atPos(start, point, in.lastOffset)(t) else t

    def atPos[T <: Positioned](start: Offset)(t: T): T =
      atPos(start, start)(t)

    def startOffset(t: Positioned): Int =
      if (t.pos.exists) t.pos.start else in.offset

    def pointOffset(t: Positioned): Int =
      if (t.pos.exists) t.pos.point else in.offset

    def endOffset(t: Positioned): Int =
      if (t.pos.exists) t.pos.end else in.lastOffset

    def nameStart: Offset =
      if (in.token == BACKQUOTED_IDENT) in.offset + 1 else in.offset

    def sourcePos(off: Int = in.offset): SourcePosition =
      source atPos Position(off)

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
        val length = if (in.name != null) in.name.show.length else 0
        syntaxError(msg, Position(offset, offset + length))
        lastErrorOffset = in.offset
      }

    /** Unconditionally issue an error at given position, without
      *  updating lastErrorOffset.
      */
    def syntaxError(msg: => Message, pos: Position): Unit =
      ctx.error(msg, source atPos pos)
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
      canStartExpressionTokens.contains(in.token) &&
      !in.isSoftModifierInModifierPosition

    def isDefIntro(allowedMods: BitSet): Boolean =
      in.token == AT ||
      (defIntroTokens `contains` in.token) ||
      (allowedMods `contains` in.token) ||
      in.isSoftModifierInModifierPosition

    def isStatSep: Boolean =
      in.token == NEWLINE || in.token == NEWLINES || in.token == SEMI

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
      ctx.warning(msg, source atPos Position(offset))

    def deprecationWarning(msg: => Message, offset: Int = in.offset): Unit =
      ctx.deprecationWarning(msg, source atPos Position(offset))

    /** Issue an error at current offset that input is incomplete */
    def incompleteInputError(msg: => Message): Unit =
      ctx.incompleteInputError(msg, source atPos Position(in.offset))

    /** If at end of file, issue an incompleteInputError.
     *  Otherwise issue a syntax error and skip to next safe point.
     */
    def syntaxErrorOrIncomplete(msg: => Message): Unit =
      if (in.token == EOF) incompleteInputError(msg)
      else {
        syntaxError(msg)
        skip()
        lastErrorOffset = in.offset
      } // DEBUG

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
            in.nextToken() // needed to ensure progress; otherwise we might cycle forever
            accept(SEMI)
        }

    def errorTermTree: Literal = atPos(in.offset) { Literal(Constant(null)) }

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
    private def withinEnum[T](isEnum: Boolean)(body: => T): T = {
      val saved = inEnum
      inEnum = isEnum
      try body
      finally inEnum = saved
    }

    def migrationWarningOrError(msg: String, offset: Int = in.offset): Unit =
      if (in.isScala2Mode)
        ctx.migrationWarning(msg, source atPos Position(offset))
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
    def convertToParam(tree: Tree, mods: Modifiers = Modifiers(), expected: String = "formal parameter"): ValDef = tree match {
      case Ident(name) =>
        makeParameter(name.asTermName, TypeTree(), mods) withPos tree.pos
      case Typed(Ident(name), tpt) =>
        makeParameter(name.asTermName, tpt, mods) withPos tree.pos
      case _ =>
        syntaxError(s"not a legal $expected", tree.pos)
        makeParameter(nme.ERROR, tree, mods)
    }

    /** Convert (qual)ident to type identifier
     */
    def convertToTypeId(tree: Tree): Tree = tree match {
      case id @ Ident(name) =>
        cpy.Ident(id)(name.toTypeName)
      case id @ Select(qual, name) =>
        cpy.Select(id)(qual, name.toTypeName)
      case _ =>
        syntaxError(IdentifierExpected(tree.show), tree.pos)
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
          case vd :: _ => syntaxError(UnboundPlaceholderParameter(), vd.pos)
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

/* -------------- XML ---------------------------------------------------- */

    /** the markup parser */
    lazy val xmlp: xml.MarkupParsers.MarkupParser = new MarkupParser(this, true)

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
              atPos(opInfo.operator.pos union opInfo.operand.pos union top.pos) {
                val op = opInfo.operator
                val l = opInfo.operand
                val r = top
                if (isType && !op.isBackquoted && op.name == tpnme.raw.BAR) {
                  OrTypeTree(checkAndOrArgument(l), checkAndOrArgument(r))
                } else if (isType && !op.isBackquoted && op.name == tpnme.raw.AMP) {
                  AndTypeTree(checkAndOrArgument(l), checkAndOrArgument(r))
                } else
                  InfixOp(l, op, r)
              }
            }
          }
          else top
        }
      }
      recur(top)
    }

    /** operand { infixop operand} [postfixop],
     *  respecting rules of associativity and precedence.
     *  @param notAnOperator  a token that does not count as operator.
     *  @param maybePostfix   postfix operators are allowed.
     */
    def infixOps(
        first: Tree, canStartOperand: Token => Boolean, operand: () => Tree,
        isType: Boolean = false,
        isOperator: => Boolean = true,
        maybePostfix: Boolean = false): Tree = {
      val base = opStack
      var top = first
      while (isIdent && isOperator) {
        val op = if (isType) typeIdent() else termIdent()
        top = reduceStack(base, top, precedence(op.name), isLeftAssoc(op.name), op.name, isType)
        opStack = OpInfo(top, op, in.offset) :: opStack
        newLineOptWhenFollowing(canStartOperand)
        if (maybePostfix && !canStartOperand(in.token)) {
          val topInfo = opStack.head
          opStack = opStack.tail
          val od = reduceStack(base, topInfo.operand, 0, true, in.name, isType)
          return atPos(startOffset(od), topInfo.offset) {
            PostfixOp(od, topInfo.operator)
          }
        }
        top = operand()
      }
      reduceStack(base, top, 0, true, in.name, isType)
    }

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
    def termIdent(): Ident = atPos(in.offset) {
      makeIdent(in.token, ident())
    }

    /** Accept identifier and return Ident with its name as a type name. */
    def typeIdent(): Ident = atPos(in.offset) {
      makeIdent(in.token, ident().toTypeName)
    }

    private def makeIdent(tok: Token, name: Name) =
      if (tok == BACKQUOTED_IDENT) BackquotedIdent(name)
      else Ident(name)

    def wildcardIdent(): Ident =
      atPos(accept(USCORE)) { Ident(nme.WILDCARD) }

    def termIdentOrWildcard(): Ident =
      if (in.token == USCORE) wildcardIdent() else termIdent()

    /** Accept identifier acting as a selector on given tree `t`. */
    def selector(t: Tree): Tree =
      atPos(startOffset(t), in.offset) { Select(t, ident()) }

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
        val t = atPos(start) { This(qual) }
        if (!thisOK && in.token != DOT) syntaxError(DanglingThisInPath(), t.pos)
        dotSelectors(t, finish)
      }
      def handleSuper(qual: Ident) = {
        in.nextToken()
        val mix = mixinQualifierOpt()
        val t = atPos(start) { Super(This(qual), mix) }
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
      if (in.token == LBRACKET) inBrackets(atPos(in.offset) { typeIdent() })
      else EmptyTypeIdent

    /** StableId ::= id
     *            |  Path `.' id
     *            |  [id '.'] super [`[' id `]']`.' id
     */
    def stableId(): Tree =
      path(thisOK = false)

    /** QualId ::= id {`.' id}
    */
    def qualId(): Tree =
      dotSelectors(termIdent())

    /** SimpleExpr    ::= literal
     *                  | symbol
     *                  | null
     *  @param negOffset   The offset of a preceding `-' sign, if any.
     *                     If the literal is not negated, negOffset = in.offset.
     */
    def literal(negOffset: Int = in.offset, inPattern: Boolean = false): Tree = {
      def finish(value: Any): Tree = {
        val t = atPos(negOffset) { Literal(Constant(value)) }
        in.nextToken()
        t
      }
      val isNegated = negOffset < in.offset
      atPos(negOffset) {
        if (in.token == SYMBOLLIT) atPos(in.skipToken()) { SymbolLit(in.strVal) }
        else if (in.token == INTERPOLATIONID) interpolatedString(inPattern)
        else finish(in.token match {
          case CHARLIT                => in.charVal
          case INTLIT                 => in.intVal(isNegated).toInt
          case LONGLIT                => in.intVal(isNegated)
          case FLOATLIT               => in.floatVal(isNegated).toFloat
          case DOUBLELIT              => in.floatVal(isNegated)
          case STRINGLIT | STRINGPART => in.strVal
          case TRUE                   => true
          case FALSE                  => false
          case NULL                   => null
          case _                      =>
            syntaxErrorOrIncomplete(IllegalLiteral())
            null
        })
      }
    }

    private def interpolatedString(inPattern: Boolean = false): Tree = atPos(in.offset) {
      val segmentBuf = new ListBuffer[Tree]
      val interpolator = in.name
      in.nextToken()
      while (in.token == STRINGPART) {
        segmentBuf += Thicket(
            literal(inPattern = inPattern),
            atPos(in.offset) {
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
                ctx.error(InterpolatedStringError(), source atPos Position(in.offset))
                EmptyTree
              }
            })
      }
      if (in.token == STRINGLIT) segmentBuf += literal(inPattern = inPattern)
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
    def toplevelTyp(): Tree = checkWildcard(typ())

    /** Type        ::=  [FunArgMods] FunArgTypes `=>' Type
     *                |  HkTypeParamClause `->' Type
     *                |  InfixType
     *  FunArgTypes ::=  InfixType
     *                |  `(' [ FunArgType {`,' FunArgType } ] `)'
     *                |  '(' TypedFunParam {',' TypedFunParam } ')'
     */
    def typ(): Tree = {
      val start = in.offset
      val imods = modifiers(funArgMods)
      def functionRest(params: List[Tree]): Tree =
        atPos(start, accept(ARROW)) {
          val t = typ()
          if (imods.is(Implicit) || imods.is(Erased)) new FunctionWithMods(params, t, imods)
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
            if (imods.is(Implicit) || isValParamList || in.token == ARROW) functionRest(ts)
            else {
              val ts1 =
                for (t <- ts) yield {
                  t match {
                    case t@ByNameTypeTree(t1) =>
                      syntaxError(ByNameParameterNotSupported(t), t.pos)
                      t1
                    case _ =>
                      t
                  }
                }
              val tuple = atPos(start) { makeTupleOrParens(ts) }
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
          if (in.token == ARROW)
            atPos(start, in.skipToken())(LambdaTypeTree(tparams, toplevelTyp()))
          else { accept(ARROW); typ() }
        }
        else infixType()

      in.token match {
        case ARROW => functionRest(t :: Nil)
        case MATCH => matchType(EmptyTree, t)
        case FORSOME => syntaxError(ExistentialTypesNoLongerSupported()); t
        case _ =>
          if (imods.is(Implicit) && !t.isInstanceOf[FunctionWithMods])
            syntaxError("Types with implicit keyword can only be function types", implicitKwPos(start))
          t
      }
    }

    private def implicitKwPos(start: Int): Position =
      Position(start, start + nme.IMPLICITkw.asSimpleName.length)

    /** TypedFunParam   ::= id ':' Type */
    def typedFunParam(start: Offset, name: TermName, mods: Modifiers = EmptyModifiers): Tree = atPos(start) {
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
      if (in.token == LBRACE) refinedTypeRest(atPos(startOffset(t)) { RefinedTypeTree(checkWildcard(t), refinement()) })
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
        AndTypeTree(checkAndOrArgument(t), checkAndOrArgument(withType()))
      }
      else t

    /** AnnotType ::= SimpleType {Annotation}
     */
    def annotType(): Tree = annotTypeRest(simpleType())

    def annotTypeRest(t: Tree): Tree =
      if (in.token == AT) annotTypeRest(atPos(startOffset(t)) { Annotated(t, annot()) })
      else t

    /** SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' id
     *                     |  StableId
     *                     |  ['~'] StableId
     *                     |  Path `.' type
     *                     |  `(' ArgTypes `)'
     *                     |  `_' TypeBounds
     *                     |  Refinement
     *                     |  Literal
     */
    def simpleType(): Tree = simpleTypeRest {
      if (in.token == LPAREN)
        atPos(in.offset) {
          makeTupleOrParens(inParens(argTypes(namedOK = false, wildOK = true)))
        }
      else if (in.token == LBRACE)
        atPos(in.offset) { RefinedTypeTree(EmptyTree, refinement()) }
      else if (isSimpleLiteral) { SingletonTypeTree(literal()) }
      else if (in.token == USCORE) {
        val start = in.skipToken()
        typeBounds().withPos(Position(start, in.lastOffset, start))
      }
      else if (isIdent(nme.raw.TILDE) && in.lookaheadIn(BitSet(IDENTIFIER, BACKQUOTED_IDENT)))
        atPos(in.offset) { PrefixOp(typeIdent(), path(thisOK = true)) }
      else path(thisOK = false, handleSingletonType) match {
        case r @ SingletonTypeTree(_) => r
        case r => convertToTypeId(r)
      }
    }

    val handleSingletonType: Tree => Tree = t =>
      if (in.token == TYPE) {
        in.nextToken()
        atPos(startOffset(t)) { SingletonTypeTree(t) }
      } else t

    private def simpleTypeRest(t: Tree): Tree = in.token match {
      case HASH => simpleTypeRest(typeProjection(t))
      case LBRACKET => simpleTypeRest(atPos(startOffset(t)) {
        AppliedTypeTree(checkWildcard(t), typeArgs(namedOK = false, wildOK = true)) })
      case _ => t
    }

    private def typeProjection(t: Tree): Tree = {
      accept(HASH)
      val id = typeIdent()
      atPos(startOffset(t), startOffset(id)) { Select(t, id.name) }
    }

    /** NamedTypeArg      ::=  id `=' Type
     */
    val namedTypeArg: () => NamedArg = () => {
      val name = ident()
      accept(EQUALS)
      NamedArg(name.toTypeName, typ())
    }

    /**   ArgTypes          ::=  Type {`,' Type}
     *                        |  NamedTypeArg {`,' NamedTypeArg}
     */
    def argTypes(namedOK: Boolean, wildOK: Boolean): List[Tree] = {
      def otherArgs(first: Tree, arg: () => Tree): List[Tree] = {
        val rest =
          if (in.token == COMMA) {
            in.nextToken()
            commaSeparated(arg)
          }
          else Nil
        first :: rest
      }
      def typParser() = checkWildcard(typ(), wildOK)
      if (namedOK && in.token == IDENTIFIER)
        typParser() match {
          case Ident(name) if in.token == EQUALS =>
            in.nextToken()
            otherArgs(NamedArg(name, typ()), namedTypeArg)
          case firstArg =>
            otherArgs(firstArg, () => typ())
        }
      else commaSeparated(() => typParser())
    }

    /** FunArgType ::=  Type | `=>' Type
     */
    val funArgType: () => Tree = () =>
      if (in.token == ARROW) atPos(in.skipToken()) { ByNameTypeTree(typ()) }
      else typ()

    /** ParamType ::= [`=>'] ParamValueType
     */
    def paramType(): Tree =
      if (in.token == ARROW) atPos(in.skipToken()) { ByNameTypeTree(paramValueType()) }
      else paramValueType()

    /** ParamValueType ::= Type [`*']
     */
    def paramValueType(): Tree = {
      val t = toplevelTyp()
      if (isIdent(nme.raw.STAR)) {
        in.nextToken()
        atPos(startOffset(t)) { PostfixOp(t, Ident(tpnme.raw.STAR)) }
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
      atPos(in.offset) { TypeBoundsTree(bound(SUPERTYPE), bound(SUBTYPE)) }

    private def bound(tok: Int): Tree =
      if (in.token == tok) { in.nextToken(); toplevelTyp() }
      else EmptyTree

    /** TypeParamBounds   ::=  TypeBounds {`<%' Type} {`:' Type}
     */
    def typeParamBounds(pname: TypeName): Tree = {
      val t = typeBounds()
      val cbs = contextBounds(pname)
      if (cbs.isEmpty) t
      else atPos((t.pos union cbs.head.pos).start) { ContextBounds(t, cbs) }
    }

    def contextBounds(pname: TypeName): List[Tree] = in.token match {
      case COLON =>
        atPos(in.skipToken()) {
          AppliedTypeTree(toplevelTyp(), Ident(pname))
        } :: contextBounds(pname)
      case VIEWBOUND =>
        deprecationWarning("view bounds `<%' are deprecated, use a context bound `:' instead")
        atPos(in.skipToken()) {
          Function(Ident(pname) :: Nil, toplevelTyp())
        } :: contextBounds(pname)
      case _ =>
        Nil
    }

    def typedOpt(): Tree =
      if (in.token == COLON) { in.nextToken(); toplevelTyp() }
      else TypeTree()

    def typeDependingOn(location: Location.Value): Tree =
      if (location == Location.InParens) typ()
      else if (location == Location.InPattern) refinedType()
      else infixType()

    /** Checks whether `t` represents a non-value type (wildcard types, or ByNameTypeTree).
     *  If it is, returns the [[Tree]] which immediately represents the non-value type.
     */
    @tailrec
    private final def findNonValueTypeTree(t: Tree, alsoNonValue: Boolean): Option[Tree] = t match {
      case TypeBoundsTree(_, _) => Some(t)
      case ByNameTypeTree(_) if alsoNonValue => Some(t)
      case Parens(t1) => findNonValueTypeTree(t1, alsoNonValue)
      case Annotated(t1, _) => findNonValueTypeTree(t1, alsoNonValue)
      case _ => None
    }

    def rejectWildcard(t: Tree, fallbackTree: Tree): Tree =
      findNonValueTypeTree(t, false) match {
        case Some(wildcardTree) =>
          syntaxError(UnboundWildcardType(), wildcardTree.pos)
          fallbackTree
        case None => t
      }


    def checkWildcard(t: Tree, wildOK: Boolean = false, fallbackTree: Tree = scalaAny): Tree =
      if (wildOK)
        t
      else
        rejectWildcard(t, fallbackTree)

    def checkAndOrArgument(t: Tree): Tree =
      findNonValueTypeTree(t, true) match {
        case Some(typTree) =>
          typTree match {
            case typTree: TypeBoundsTree =>
              syntaxError(UnboundWildcardType(), typTree.pos)
            case typTree: ByNameTypeTree =>
              syntaxError(ByNameParameterNotSupported(typTree), typTree.pos)
          }
          scalaAny
        case None => t
      }

/* ----------- EXPRESSIONS ------------------------------------------------ */

    /** EqualsExpr ::= `=' Expr
     */
    def equalsExpr(): Tree = {
      accept(EQUALS)
      expr()
    }

    def condExpr(altToken: Token): Tree = {
      if (in.token == LPAREN) {
        val t = atPos(in.offset) { Parens(inParens(exprInParens())) }
        if (in.token == altToken) in.nextToken()
        t
      } else {
        val t = expr()
        accept(altToken)
        t
      }
    }

    /** Expr              ::=  [FunArgMods] FunParams =>' Expr
     *                      |  Expr1
     *  FunParams         ::=  Bindings
     *                      |  id
     *                      |  `_'
     *  ExprInParens      ::=  PostfixExpr `:' Type
     *                      |  Expr
     *  BlockResult       ::=  [FunArgMods] FunParams =>' Block
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
     *                      |  [SimpleExpr `.'] id `=' Expr
     *                      |  SimpleExpr1 ArgumentExprs `=' Expr
     *                      |  PostfixExpr [Ascription]
     *                      |  [‘inline’] PostfixExpr `match' `{' CaseClauses `}'
     *                      |  `implicit' `match' `{' ImplicitCaseClauses `}'
     *  Bindings          ::= `(' [Binding {`,' Binding}] `)'
     *  Binding           ::= (id | `_') [`:' Type]
     *  Ascription        ::= `:' CompoundType
     *                      | `:' Annotation {Annotation}
     *                      | `:' `_' `*'
     */
    val exprInParens: () => Tree = () => expr(Location.InParens)

    def expr(): Tree = expr(Location.ElseWhere)

    def expr(location: Location.Value): Tree = {
      val start = in.offset
      if (in.token == IMPLICIT || in.token == ERASED) {
        val imods = modifiers(funArgMods)
        if (in.token == MATCH) implicitMatch(start, imods)
        else implicitClosure(start, location, imods)
      } else {
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
        atPos(in.skipToken()) {
          val cond = condExpr(DO)
          newLinesOpt()
          val body = expr()
          WhileDo(cond, body)
        }
      case DO =>
        atPos(in.skipToken()) {
          val body = expr()
          if (isStatSep) in.nextToken()
          accept(WHILE)
          val cond = expr()
          DoWhile(body, cond)
        }
      case TRY =>
        val tryOffset = in.offset
        atPos(in.skipToken()) {
          val body = expr()
          val (handler, handlerStart) =
            if (in.token == CATCH) {
              val pos = in.offset
              in.nextToken()
              (expr(), pos)
            } else (EmptyTree, -1)

          handler match {
            case Block(Nil, EmptyTree) =>
              assert(handlerStart != -1)
              syntaxError(
                EmptyCatchBlock(body),
                Position(handlerStart, endOffset(handler))
              )
            case _ =>
          }

          val finalizer =
            if (in.token == FINALLY) { accept(FINALLY); expr() }
            else {
              if (handler.isEmpty) warning(
                EmptyCatchAndFinallyBlock(body),
                source atPos Position(tryOffset, endOffset(body))
              )
              EmptyTree
            }
          ParsedTry(body, handler, finalizer)
        }
      case THROW =>
        atPos(in.skipToken()) { Throw(expr()) }
      case RETURN =>
        atPos(in.skipToken()) { Return(if (isExprIntro) expr() else EmptyTree, EmptyTree) }
      case FOR =>
        forExpr()
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
             atPos(startOffset(t), in.skipToken()) { Assign(t, expr()) }
           case _ =>
             t
         }
      case COLON =>
        ascription(t, location)
      case MATCH =>
        matchExpr(t, startOffset(t), Match)
      case _ =>
        t
    }

    def ascription(t: Tree, location: Location.Value): Tree = atPos(startOffset(t)) {
      in.skipToken()
      in.token match {
        case USCORE =>
          val uscoreStart = in.skipToken()
          if (isIdent(nme.raw.STAR)) {
            in.nextToken()
            if (in.token != RPAREN) syntaxError(SeqWildcardPatternPos(), uscoreStart)
            Typed(t, atPos(uscoreStart) { Ident(tpnme.WILDCARD_STAR) })
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
              cpy.ValDef(vd)(tpt = tpt).withPos(vd.pos union tpt.pos) :: rest
          }
          Typed(t, tpt)
      }
    }

    /**    `if' `(' Expr `)' {nl} Expr [[semi] else Expr]
     *     `if' Expr `then' Expr [[semi] else Expr]
     */
    def ifExpr(start: Offset, mkIf: (Tree, Tree, Tree) => If): If =
      atPos(start, in.skipToken()) {
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
      atPos(start, in.skipToken()) {
        inBraces(mkMatch(t, caseClauses(caseClause)))
      }

    /**    `match' { ImplicitCaseClauses }
     */
    def implicitMatch(start: Int, imods: Modifiers) = {
      def markFirstIllegal(mods: List[Mod]) = mods match {
        case mod :: _ => syntaxError(em"illegal modifier for implicit match", mod.pos)
        case _ =>
      }
      imods.mods match {
        case Mod.Implicit() :: mods => markFirstIllegal(mods)
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
          syntaxError(em"not a legal pattern for an implicit match", pat.pos)
      }
      result
    }

    /**    `match' { TypeCaseClauses }
     */
    def matchType(bound: Tree, t: Tree): MatchTypeTree =
      atPos((if (bound.isEmpty) t else bound).pos.start, accept(MATCH)) {
        inBraces(MatchTypeTree(bound, t, caseClauses(typeCaseClause)))
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
              patch(source, Position(start), "(")
              patch(source, Position(in.lastOffset), ")")
            }
            t
          }
          else TypeTree()
        (atPos(start) { makeParameter(name, t, mods) }) :: Nil
      }

    /**  Binding           ::= (id | `_') [`:' Type]
     */
    def binding(mods: Modifiers): Tree =
      atPos(in.offset) { makeParameter(bindingName(), typedOpt(), mods) }

    def bindingName(): TermName =
      if (in.token == USCORE) {
        in.nextToken()
        WildcardParamName.fresh()
      }
      else ident()

    /** Expr         ::= implicit id `=>' Expr
     *  BlockResult  ::= implicit id [`:' InfixType] `=>' Block // Scala2 only
     */
    def implicitClosure(start: Int, location: Location.Value, implicitMods: Modifiers): Tree =
      closureRest(start, location, funParams(implicitMods, location))

    def closureRest(start: Int, location: Location.Value, params: List[Tree]): Tree =
      atPos(start, in.offset) {
        accept(ARROW)
        Function(params, if (location == Location.InBlock) block() else expr())
      }

    /** PostfixExpr   ::= InfixExpr [id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr id [nl] InfixExpr
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
          atPos(start) { PrefixOp(op, simpleExpr()) }
      }
      else simpleExpr()

    /** SimpleExpr    ::= new Template
     *                 |  BlockExpr
     *                 |  ‘'{’ BlockExprContents ‘}’
     *                 |  ‘'(’ ExprsInParens ‘)’
     *                 |  ‘'[’ Type ‘]’
     *                 |  SimpleExpr1 [`_']
     *  SimpleExpr1   ::= literal
     *                 |  xmlLiteral
     *                 |  Path
     *                 |  `(' [ExprsInParens] `)'
     *                 |  SimpleExpr `.' id
     *                 |  SimpleExpr (TypeArgs | NamedTypeArgs)
     *                 |  SimpleExpr1 ArgumentExprs
     */
    def simpleExpr(): Tree = {
      var canApply = true
      val t = in.token match {
        case XMLSTART =>
          xmlLiteral()
        case IDENTIFIER | BACKQUOTED_IDENT | THIS | SUPER =>
          path(thisOK = true)
        case USCORE =>
          val start = in.skipToken()
          val pname = WildcardParamName.fresh()
          val param = ValDef(pname, TypeTree(), EmptyTree).withFlags(SyntheticTermParam)
            .withPos(Position(start))
          placeholderParams = param :: placeholderParams
          atPos(start) { Ident(pname) }
        case LPAREN =>
          atPos(in.offset) { makeTupleOrParens(inParens(exprsInParensOpt())) }
        case LBRACE =>
          canApply = false
          blockExpr()
        case QPAREN =>
          in.token = LPAREN
          atPos(in.offset)(Quote(simpleExpr()))
        case QBRACE =>
          in.token = LBRACE
          atPos(in.offset)(Quote(simpleExpr()))
        case QBRACKET =>
          in.token = LBRACKET
          atPos(in.offset)(Quote(inBrackets(typ())))
        case NEW =>
          canApply = false
          val start = in.skipToken()
          val (impl, missingBody) = template(emptyConstructor)
          impl.parents match {
            case parent :: Nil if missingBody =>
              if (parent.isType) ensureApplied(wrapNew(parent))
              else parent.withPos(Position(start, in.lastOffset))
            case _ =>
              New(impl.withPos(Position(start, in.lastOffset)))
          }
        case _ =>
          if (isLiteral) literal()
          else {
            syntaxErrorOrIncomplete(IllegalStartSimpleExpr(tokenString(in.token)))
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
          val tapp = atPos(startOffset(t), in.offset) { TypeApply(t, typeArgs(namedOK = true, wildOK = false)) }
          simpleExprRest(tapp, canApply = true)
        case LPAREN | LBRACE if canApply =>
          val app = atPos(startOffset(t), in.offset) { Apply(t, argumentExprs()) }
          simpleExprRest(app, canApply = true)
        case USCORE =>
          atPos(startOffset(t), in.skipToken()) { PostfixOp(t, Ident(nme.WILDCARD)) }
        case _ =>
          t
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
          atPos(startOffset(fn)) { Apply(fn, parArgumentExprs()) }
        )
      else fn
    }

    /** BlockExpr         ::= `{' BlockExprContents `}'
     *  BlockExprContents ::= CaseClauses | Block
     */
    def blockExpr(): Tree = atPos(in.offset) {
      inDefScopeBraces {
        if (in.token == CASE) Match(EmptyTree, caseClauses(caseClause))
        else block()
      }
    }

    /** Block ::= BlockStatSeq
     *  @note  Return tree does not carry source position.
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
      else {
        val pat = pattern1()
        if (in.token == EQUALS) atPos(startOffset(pat), in.skipToken()) { GenAlias(pat, expr()) }
        else generatorRest(pat)
      }

    /** Generator   ::=  Pattern `<-' Expr
     */
    def generator(): Tree = generatorRest(pattern1())

    def generatorRest(pat: Tree): GenFrom =
      atPos(startOffset(pat), accept(LARROW)) { GenFrom(pat, expr()) }

    /** ForExpr  ::= `for' (`(' Enumerators `)' | `{' Enumerators `}')
     *                {nl} [`yield'] Expr
     *            |  `for' Enumerators (`do' Expr | `yield' Expr)
     */
    def forExpr(): Tree = atPos(in.skipToken()) {
      var wrappedEnums = true
      val enums =
        if (in.token == LBRACE) inBraces(enumerators())
        else if (in.token == LPAREN) {
          val lparenOffset = in.skipToken()
          openParens.change(LPAREN, 1)
          val pats = patternsOpt()
          val pat =
            if (in.token == RPAREN || pats.length > 1) {
              wrappedEnums = false
              accept(RPAREN)
              openParens.change(LPAREN, -1)
              atPos(lparenOffset) { makeTupleOrParens(pats) } // note: alternatives `|' need to be weeded out by typer.
            }
            else pats.head
          val res = generatorRest(pat) :: enumeratorsRest()
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
    val caseClause: () => CaseDef = () => atPos(in.offset) {
      accept(CASE)
      CaseDef(pattern(), guard(), atPos(accept(ARROW)) { block() })
    }

    /** TypeCaseClause     ::= ‘case’ InfixType ‘=>’ Type [nl]
     */
    val typeCaseClause: () => CaseDef = () => atPos(in.offset) {
      accept(CASE)
      CaseDef(infixType(), EmptyTree, atPos(accept(ARROW)) {
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
        atPos(startOffset(pat)) { Alternative(pat :: patternAlts()) }
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
      if (isVarPattern(p) && in.token == COLON) ascription(p, Location.InPattern)
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
            migrationWarningOrError("The syntax `x @ _*' is no longer supported; use `x : _*' instead", startOffset(p))
            atPos(startOffset(p), offset) { Typed(p, pt) }
          case p =>
            atPos(startOffset(p), offset) { Bind(name, p) }
        }
      case p @ Ident(tpnme.WILDCARD_STAR) =>
        // compatibility for Scala2 `_*` syntax
        migrationWarningOrError("The syntax `_*' is no longer supported; use `x : _*' instead", startOffset(p))
        atPos(startOffset(p)) { Typed(Ident(nme.WILDCARD), p) }
      case p =>
        p
    }

    /**  InfixPattern ::= SimplePattern {id [nl] SimplePattern}
     */
    def infixPattern(): Tree =
      infixOps(simplePattern(), canStartExpressionTokens, simplePattern, isOperator = in.name != nme.raw.BAR)

    /** SimplePattern    ::= PatVar
     *                    |  Literal
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
          if (in.token != RPAREN) syntaxError(SeqWildcardPatternPos(), wildIndent.pos)
          atPos(wildIndent.pos) { Ident(tpnme.WILDCARD_STAR) }
        } else wildIndent
      case LPAREN =>
        atPos(in.offset) { makeTupleOrParens(inParens(patternsOpt())) }
      case XMLSTART =>
        xmlLiteralPattern()
      case _ =>
        if (isLiteral) literal(inPattern = true)
        else {
          syntaxErrorOrIncomplete(IllegalStartOfSimplePattern())
          errorTermTree
        }
    }

    def simplePatternRest(t: Tree): Tree = {
      var p = t
      if (in.token == LBRACKET)
        p = atPos(startOffset(t), in.offset) { TypeApply(p, typeArgs(namedOK = false, wildOK = false)) }
      if (in.token == LPAREN)
        p = atPos(startOffset(t), in.offset) { Apply(p, argumentPatterns()) }
      p
    }

    /** Patterns          ::=  Pattern [`,' Pattern]
     */
    def patterns(): List[Tree] = commaSeparated(pattern)

    def patternsOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else patterns()


    /** ArgumentPatterns  ::=  `(' [Patterns] `)'
     *                      |  `(' [Patterns `,'] Pattern2 `:' `_' `*' ')
     */
    def argumentPatterns(): List[Tree] =
      inParens(patternsOpt())

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    private def modOfToken(tok: Int, name: Name): Mod = tok match {
      case ABSTRACT    => Mod.Abstract()
      case FINAL       => Mod.Final()
      case IMPLICIT    => Mod.Implicit()
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
      val mod = atPos(in.skipToken()) { modOfToken(tok, name) }

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

    /** FunArgMods ::= { `implicit` | `erased` }
     */
    def funArgMods: BitSet = BitSet(IMPLICIT, ERASED)

    /** Wrap annotation or constructor in New(...).<init> */
    def wrapNew(tpt: Tree): Select = Select(New(tpt), nme.CONSTRUCTOR)

    /** Adjust start of annotation or constructor to position of preceding @ or new */
    def adjustStart(start: Offset)(tree: Tree): Tree = {
      val tree1 = tree match {
        case Apply(fn, args) => cpy.Apply(tree)(adjustStart(start)(fn), args)
        case Select(qual, name) => cpy.Select(tree)(adjustStart(start)(qual), name)
        case _ => tree
      }
      if (tree1.pos.exists && start < tree1.pos.start)
        tree1.withPos(tree1.pos.withStart(start))
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

    /** ClsTypeParamClause::=  `[' ClsTypeParam {`,' ClsTypeParam} `]'
     *  ClsTypeParam      ::=  {Annotation} [`+' | `-']
     *                         id [HkTypeParamClause] TypeParamBounds
     *
     *  DefTypeParamClause::=  `[' DefTypeParam {`,' DefTypeParam} `]'
     *  DefTypeParam      ::=  {Annotation} id [HkTypeParamClause] TypeParamBounds
     *
     *  TypTypeParamCaluse::=  `[' TypTypeParam {`,' TypTypeParam} `]'
     *  TypTypeParam      ::=  {Annotation} id [HkTypePamClause] TypeBounds
     *
     *  HkTypeParamClause ::=  `[' HkTypeParam {`,' HkTypeParam} `]'
     *  HkTypeParam       ::=  {Annotation} ['+' | `-'] (id [HkTypePamClause] | _') TypeBounds
     */
    def typeParamClause(ownerKind: ParamOwner.Value): List[TypeDef] = inBrackets {
      def typeParam(): TypeDef = {
        val isConcreteOwner = ownerKind == ParamOwner.Class || ownerKind == ParamOwner.Def
        val start = in.offset
        val mods = atPos(start) {
          annotsAsMods() | {
            if (ownerKind == ParamOwner.Class) Param | PrivateLocal
            else Param
          } | {
            if (ownerKind != ParamOwner.Def)
              if (isIdent(nme.raw.PLUS)) { in.nextToken(); Covariant }
              else if (isIdent(nme.raw.MINUS)) { in.nextToken(); Contravariant }
              else EmptyFlags
            else EmptyFlags
          }
        }
        atPos(start, nameStart) {
          val name =
            if (isConcreteOwner || in.token != USCORE) ident().toTypeName
            else {
              in.nextToken()
              WildcardParamName.fresh().toTypeName
            }
          val hkparams = typeParamClauseOpt(ParamOwner.TypeParam)
          val bounds =
            if (isConcreteOwner) typeParamBounds(name)
            else typeBounds()
          TypeDef(name, lambdaAbstract(hkparams, bounds)).withMods(mods)
        }
      }
      commaSeparated(() => typeParam())
    }

    def typeParamClauseOpt(ownerKind: ParamOwner.Value): List[TypeDef] =
      if (in.token == LBRACKET) typeParamClause(ownerKind) else Nil

    /** ClsParamClauses   ::=  {ClsParamClause} [[nl] `(' [FunArgMods] ClsParams `)']
     *  ClsParamClause    ::=  [nl] `(' [`erased'] [ClsParams] ')'
     *  ClsParams         ::=  ClsParam {`' ClsParam}
     *  ClsParam          ::=  {Annotation} [{Modifier} (`val' | `var') | `inline'] Param
     *  DefParamClauses   ::=  {DefParamClause} [[nl] `(' [FunArgMods] DefParams `)']
     *  DefParamClause    ::=  [nl] `(' [`erased'] [DefParams] ')'
     *  DefParams         ::=  DefParam {`,' DefParam}
     *  DefParam          ::=  {Annotation} [`inline'] Param
     *  Param             ::=  id `:' ParamType [`=' Expr]
     *
     *  @return   the list of parameter definitions
     */
    def paramClause(ofClass: Boolean = false,                // owner is a class
                    ofCaseClass: Boolean = false,            // owner is a case class
                    ofMethod: Boolean = false,               // owner is a method or constructor
                    prefix: Boolean = false,                 // clause precedes name of an extension method
                    firstClause: Boolean = false)            // clause is the first in regular list of clauses
                    : List[ValDef] = {
      var implicitOffset = -1 // use once

      def param(impliedMods: Modifiers): ValDef = {
        val start = in.offset
        var mods = impliedMods.withAnnotations(annotations())
        if (ofClass) {
          mods = addFlag(modifiers(start = mods), ParamAccessor)
          mods =
            atPos(start, in.offset) {
              if (in.token == VAL) {
                in.nextToken()
                mods
              }
              else if (in.token == VAR) {
                val mod = atPos(in.skipToken()) { Mod.Var() }
                addMod(mods, mod)
              }
              else {
                if (!(mods.flags &~ (ParamAccessor | Inline | impliedMods.flags)).isEmpty)
                  syntaxError("`val' or `var' expected")
                if (firstClause && ofCaseClass) mods
                else mods | PrivateLocal
              }
            }
        }
        else {
          if (isIdent(nme.inline) && in.isSoftModifierInParamModifierPosition)
            mods = addModifier(mods)
          mods = atPos(start) { mods | Param }
        }
        atPos(start, nameStart) {
          val name = ident()
          accept(COLON)
          if (in.token == ARROW && ofClass && !(mods is Local))
            syntaxError(VarValParametersMayNotBeCallByName(name, mods is Mutable))
          val tpt = paramType()
          val default =
            if (in.token == EQUALS) { in.nextToken(); expr() }
            else EmptyTree
          if (implicitOffset >= 0) {
            mods = mods.withPos(mods.pos.union(Position(implicitOffset, implicitOffset)))
            implicitOffset = -1
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
              syntaxError(VarArgsParamMustComeLast(), vparam.tpt.pos)
            case _ =>
          }
          checkVarArgsRules(rest)
      }

      // begin paramClause
      inParens {
        if (in.token == RPAREN && !prefix) Nil
        else {
          def funArgMods(mods: Modifiers): Modifiers =
            if (in.token == IMPLICIT) {
              implicitOffset = in.offset
              funArgMods(addMod(mods, atPos(accept(IMPLICIT)) { Mod.Implicit() }))
            }
            else if (in.token == ERASED)
              funArgMods(addMod(mods, atPos(accept(ERASED)) { Mod.Erased() }))
            else mods

          val paramMods = funArgMods(EmptyModifiers)
          val clause =
            if (prefix) param(paramMods) :: Nil
            else commaSeparated(() => param(paramMods))
          checkVarArgsRules(clause)
          clause
        }
      }
    }

    /** ClsParamClauses   ::=  {ClsParamClause}
     *  DefParamClauses   ::=  {DefParamClause}
     *
     *  @return  The parameter definitions
     */
    def paramClauses(ofClass: Boolean = false,
                     ofCaseClass: Boolean = false,
                     ofMethod: Boolean = false): (List[List[ValDef]]) = {
      def recur(firstClause: Boolean): List[List[ValDef]] = {
        newLineOptWhenFollowedBy(LPAREN)
        if (in.token == LPAREN) {
          val params = paramClause(
              ofClass = ofClass,
              ofCaseClass = ofCaseClass,
              ofMethod = ofMethod,
              firstClause = firstClause)
          val lastClause =
            params.nonEmpty && params.head.mods.flags.is(Implicit)
          params :: (if (lastClause) Nil else recur(firstClause = false))
        }
        else Nil
      }
      recur(firstClause = true)
    }

/* -------- DEFS ------------------------------------------- */

    def finalizeDef(md: MemberDef, mods: Modifiers, start: Int): md.ThisTree[Untyped] =
      md.withMods(mods).setComment(in.getDocComment(start))

    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    def importClause(): List[Tree] = {
      val offset = accept(IMPORT)
      commaSeparated(importExpr) match {
        case t :: rest =>
          // The first import should start at the position of the keyword.
          val firstPos =
            if (t.pos.exists) t.pos.withStart(offset)
            else Position(offset, in.lastOffset)
          t.withPos(firstPos) :: rest
        case nil => nil
      }
    }

    /**  ImportExpr ::= StableId `.' (id | `_' | ImportSelectors)
     */
    val importExpr: () => Import = () => path(thisOK = false, handleImport) match {
      case imp: Import =>
        imp
      case sel @ Select(qual, name) =>
        val selector = atPos(pointOffset(sel)) { Ident(name) }
        cpy.Import(sel)(qual, selector :: Nil)
      case t =>
        accept(DOT)
        Import(t, Ident(nme.WILDCARD) :: Nil)
    }

    val handleImport: Tree => Tree = { tree: Tree =>
      if (in.token == USCORE) Import(tree, importSelector() :: Nil)
      else if (in.token == LBRACE) Import(tree, inBraces(importSelectors()))
      else tree
    }

    /** ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     */
    def importSelectors(): List[Tree] = {
      val sel = importSelector()
      if (in.token == RBRACE) sel :: Nil
      else {
        sel :: {
          if (!isWildcardArg(sel) && in.token == COMMA) {
            in.nextToken()
            importSelectors()
          }
          else Nil
        }
      }
    }

    /** ImportSelector ::= id [`=>' id | `=>' `_']
     */
    def importSelector(): Tree = {
      val from = termIdentOrWildcard()
      if (from.name != nme.WILDCARD && in.token == ARROW)
        atPos(startOffset(from), in.skipToken()) {
          val start = in.offset
          val to = termIdentOrWildcard()
          val toWithPos =
            if (to.name == nme.ERROR)
              // error identifiers don't consume any characters, so atPos(start)(id) wouldn't set a position.
              // Some testcases would then fail in Positioned.checkPos. Set a position anyway!
              atPos(start, start, in.lastOffset)(to)
            else
              to
          Thicket(from, toWithPos)
        }
      else from
    }

    def posMods(start: Int, mods: Modifiers): Modifiers = {
      val mods1 = atPos(start)(mods)
      in.nextToken()
      mods1
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
        val mod = atPos(in.skipToken()) { Mod.Var() }
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

    /** PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  VarDef ::= PatDef | id {`,' id} `:' Type `=' `_'
     *  ValDcl ::= id {`,' id} `:' Type
     *  VarDcl ::= id {`,' id} `:' Type
     */
    def patDefOrDcl(start: Offset, mods: Modifiers): Tree = atPos(start, nameStart) {
      val lhs = commaSeparated(pattern2)
      val tpt = typedOpt()
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
        case (id @ Ident(name: TermName)) :: Nil => {
          finalizeDef(ValDef(name, tpt, rhs), mods, start)
        } case _ =>
          PatDef(mods, lhs, tpt, rhs)
      }
    }

    /** DefDef ::= DefSig [(‘:’ | ‘<:’) Type] ‘=’ Expr
     *           | this ParamClause ParamClauses `=' ConstrExpr
     *  DefDcl ::= DefSig `:' Type
     *  DefSig ::= ‘(’ DefParam ‘)’ [nl] id [DefTypeParamClause] ParamClauses
     */
    def defDefOrDcl(start: Offset, mods: Modifiers): Tree = atPos(start, nameStart) {
      def scala2ProcedureSyntax(resultTypeStr: String) = {
        val toInsert =
          if (in.token == LBRACE) s"$resultTypeStr ="
          else ": Unit "  // trailing space ensures that `def f()def g()` works.
        in.testScala2Mode(s"Procedure syntax no longer supported; `$toInsert' should be inserted here") && {
          patch(source, Position(in.lastOffset), toInsert)
          true
        }
      }
      if (in.token == THIS) {
        in.nextToken()
        val vparamss = paramClauses()
        if (vparamss.isEmpty || vparamss.head.take(1).exists(_.mods.is(Implicit)))
          in.token match {
            case LBRACKET   => syntaxError("no type parameters allowed here")
            case EOF        => incompleteInputError(AuxConstructorNeedsNonImplicitParameter())
            case _          => syntaxError(AuxConstructorNeedsNonImplicitParameter(), nameStart)
          }
        if (in.isScala2Mode) newLineOptWhenFollowedBy(LBRACE)
        val rhs = {
          if (!(in.token == LBRACE && scala2ProcedureSyntax(""))) accept(EQUALS)
          atPos(in.offset) { constrExpr() }
        }
        makeConstructor(Nil, vparamss, rhs).withMods(mods).setComment(in.getDocComment(start))
      } else {
        val (leadingParamss: List[List[ValDef]], flags: FlagSet) =
          if (in.token == LPAREN)
            (paramClause(ofMethod = true, prefix = true) :: Nil, Method | Extension)
          else
            (Nil, Method)
        val mods1 = addFlag(mods, flags)
        val name = ident()
        val tparams = typeParamClauseOpt(ParamOwner.Def)
        val vparamss = leadingParamss ::: paramClauses(ofMethod = true)
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
        finalizeDef(DefDef(name, tparams, vparamss, tpt, rhs), mods1, start)
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
      atPos(accept(THIS)) {
        newLineOptWhenFollowedBy(LBRACE)
        argumentExprss(Apply(Ident(nme.CONSTRUCTOR), argumentExprs()))
      }

    /** ConstrBlock    ::=  `{' SelfInvocation {semi BlockStat} `}'
     */
    def constrBlock(): Tree =
      atPos(in.skipToken()) {
        val stats = selfInvocation() :: {
          if (isStatSep) { in.nextToken(); blockStatSeq() }
          else Nil
        }
        accept(RBRACE)
        Block(stats, Literal(Constant(())))
      }

    /** TypeDcl ::=  id [TypeParamClause] (TypeBounds | ‘=’ Type)
     *            |  id [TypeParamClause] <: Type = MatchType
     */
    def typeDefOrDcl(start: Offset, mods: Modifiers): Tree = {
      newLinesOpt()
      atPos(start, nameStart) {
        val name = ident().toTypeName
        val tparams = typeParamClauseOpt(ParamOwner.Type)
        def makeTypeDef(rhs: Tree): Tree =
          finalizeDef(TypeDef(name, lambdaAbstract(tparams, rhs)), mods, start)
        in.token match {
          case EQUALS =>
            in.nextToken()
            makeTypeDef(toplevelTyp())
          case SUBTYPE =>
            in.nextToken()
            val bound = toplevelTyp()
            if (in.token == EQUALS) {
              in.nextToken()
              makeTypeDef(matchType(bound, infixType()))
            }
            else makeTypeDef(TypeBoundsTree(EmptyTree, bound))
          case SUPERTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF =>
            makeTypeDef(typeBounds())
          case _ =>
            syntaxErrorOrIncomplete(ExpectedTypeBoundOrEquals(in.token))
            EmptyTree
        }
      }
    }

    /** TmplDef ::=  ([`case'] ‘class’ | trait’) ClassDef
     *            |  [`case'] `object' ObjectDef
     *            |  `enum' EnumDef
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
          enumDef(start, mods, atPos(in.skipToken()) { Mod.Enum() })
        case _ =>
          syntaxErrorOrIncomplete(ExpectedStartOfTopLevelDefinition())
          EmptyTree
      }
    }

    /** ClassDef ::= id ClassConstr TemplateOpt
     */
    def classDef(start: Offset, mods: Modifiers): TypeDef = atPos(start, nameStart) {
      classDefRest(start, mods, ident().toTypeName)
    }

    def classDefRest(start: Offset, mods: Modifiers, name: TypeName): TypeDef = {
      val constr = classConstr(isCaseClass = mods.is(Case))
      val templ = templateOpt(constr)
      finalizeDef(TypeDef(name, templ), mods, start)
    }

    /** ClassConstr ::= [ClsTypeParamClause] [ConstrMods] ClsParamClauses
     */
    def classConstr(isCaseClass: Boolean = false): DefDef = atPos(in.lastOffset) {
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
    def objectDef(start: Offset, mods: Modifiers): ModuleDef = atPos(start, nameStart) {
      objectDefRest(start, mods, ident())
    }

    def objectDefRest(start: Offset, mods: Modifiers, name: TermName): ModuleDef = {
      val template = templateOpt(emptyConstructor)
      finalizeDef(ModuleDef(name, template), mods, start)
    }

    /**  EnumDef ::=  id ClassConstr [`extends' [ConstrApps]] EnumBody
     */
    def enumDef(start: Offset, mods: Modifiers, enumMod: Mod): TypeDef = atPos(start, nameStart) {
      val modName = ident()
      val clsName = modName.toTypeName
      val constr = classConstr()
      val impl = templateOpt(constr, isEnum = true)
      finalizeDef(TypeDef(clsName, impl), addMod(mods, enumMod), start)
    }

    /** EnumCase = `case' (id ClassConstr [`extends' ConstrApps] | ids)
     */
    def enumCase(start: Offset, mods: Modifiers): DefTree = {
      val mods1 = addMod(mods, atPos(in.offset)(Mod.Enum())) | Case
      accept(CASE)

      in.adjustSepRegions(ARROW)
        // Scanner thinks it is in a pattern match after seeing the `case`.
        // We need to get it out of that mode by telling it we are past the `=>`

      atPos(start, nameStart) {
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
      Template(constr, parents, EmptyValDef, Nil)
    }

/* -------- TEMPLATES ------------------------------------------- */

    /** ConstrApp         ::=  SimpleType {ParArgumentExprs}
     */
    val constrApp: () => Tree = () => {
      // Using Ident(nme.ERROR) to avoid causing cascade errors on non-user-written code
      val t = checkWildcard(annotType(), fallbackTree = Ident(nme.ERROR))
      if (in.token == LPAREN) parArgumentExprss(wrapNew(t))
      else t
    }

    /** Template          ::=  ConstrApps [TemplateBody] | TemplateBody
     *  ConstrApps        ::=  ConstrApp {`with' ConstrApp}
     *
     *  @return  a pair consisting of the template, and a boolean which indicates
     *           whether the template misses a body (i.e. no {...} part).
     */
    def template(constr: DefDef, isEnum: Boolean = false): (Template, Boolean) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) (templateBodyOpt(constr, Nil, isEnum), false)
      else {
        val parents = tokenSeparated(WITH, constrApp)
        newLineOptWhenFollowedBy(LBRACE)
        if (isEnum && in.token != LBRACE)
          syntaxErrorOrIncomplete(ExpectedTokenButFound(LBRACE, in.token))
        val missingBody = in.token != LBRACE
        (templateBodyOpt(constr, parents, isEnum), missingBody)
      }
    }

    /** TemplateOpt = [`extends' Template | TemplateBody]
     */
    def templateOpt(constr: DefDef, isEnum: Boolean = false): Template =
      if (in.token == EXTENDS) { in.nextToken(); template(constr, isEnum)._1 }
      else {
        newLineOptWhenFollowedBy(LBRACE)
        if (in.token == LBRACE) template(constr, isEnum)._1
        else Template(constr, Nil, EmptyValDef, Nil)
      }

    /** TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     */
    def templateBodyOpt(constr: DefDef, parents: List[Tree], isEnum: Boolean): Template = {
      val (self, stats) =
        if (in.token == LBRACE) withinEnum(isEnum)(templateBody()) else (EmptyValDef, Nil)
      Template(constr, parents, self, stats)
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
      case x: RefTree => atPos(start, pointOffset(pkg))(PackageDef(x, stats))
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
     *  TopStat ::= Annotations Modifiers TmplDef
     *            | Packaging
     *            | package object objectDef
     *            | Import
     *            |
     */
    def topStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        setLastStatOffset()
        if (in.token == PACKAGE) {
          val start = in.skipToken()
          if (in.token == OBJECT)
            stats += objectDef(start, atPos(start, in.skipToken()) { Modifiers(Package) })
          else stats += packaging(start)
        }
        else if (in.token == IMPORT)
          stats ++= importClause()
        else if (in.token == AT || isTemplateIntro || isModifier)
          stats +++= tmplDef(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          if (in.token == CASE)
            syntaxErrorOrIncomplete(OnlyCaseClassOrCaseObjectAllowed())
          else
            syntaxErrorOrIncomplete(ExpectedClassOrObjectDef())
          if (mustStartStat) // do parse all definitions even if they are probably local (i.e. a "}" has been forgotten)
            defOrDcl(in.offset, defAnnotsMods(modifierTokens))
        }
        acceptStatSepUnlessAtEnd()
      }
      stats.toList
    }

    /** TemplateStatSeq  ::= [id [`:' Type] `=>'] TemplateStat {semi TemplateStat}
     *  TemplateStat     ::= Import
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
              self = makeSelfDef(nme.WILDCARD, tpt).withPos(first.pos)
            case _ =>
              val ValDef(name, tpt, _) = convertToParam(first, EmptyModifiers, "self type clause")
              if (name != nme.ERROR)
                self = makeSelfDef(name, tpt).withPos(first.pos)
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
          stats ++= importClause()
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
          syntaxError("illegal refinement", tree.pos)
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
        // A final modifier means the local definition is "class-like".
        tmplDef(start, mods)
      } else {
        defOrDcl(start, mods)
      }
    }

    /** BlockStatSeq ::= { BlockStat semi } [ResultExpr]
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
          stats ++= importClause()
        else if (isExprIntro)
          stats += expr(Location.InBlock)
        else if (isDefIntro(localModifierTokens))
          if (in.token == IMPLICIT || in.token == ERASED) {
            val start = in.offset
            var imods = modifiers(funArgMods)
            if (isBindingIntro)
              stats += implicitClosure(start, Location.InBlock, imods)
            else if (in.token == MATCH)
              stats += implicitMatch(start, imods)
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
            ts += objectDef(start, atPos(start, in.skipToken()) { Modifiers(Package) })
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
