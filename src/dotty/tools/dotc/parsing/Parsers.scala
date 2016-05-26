package dotty.tools
package dotc
package parsing

import scala.collection.mutable.ListBuffer
import scala.collection.immutable.BitSet
import util.{ SourceFile, SourcePosition }
import Tokens._
import Scanners._
import MarkupParsers._
import core._
import Flags._
import Contexts._
import Names._
import ast.Positioned
import ast.Trees._
import Decorators._
import StdNames._
import util.Positions._
import Constants._
import ScriptParsers._
import annotation.switch
import util.DotClass
import rewrite.Rewrites.patch

object Parsers {

  import ast.untpd._

  case class OpInfo(operand: Tree, operator: Name, offset: Offset)

  class ParensCounters {
    private var parCounts = new Array[Int](lastParen - firstParen)

    def count(tok: Token) = parCounts(tok - firstParen)
    def change(tok: Token, delta: Int) = parCounts(tok - firstParen) += delta
    def nonePositive: Boolean = parCounts forall (_ <= 0)
  }

  @sharable object Location extends Enumeration {
    val InParens, InBlock, InPattern, ElseWhere = Value
  }

  @sharable object ParamOwner extends Enumeration {
    val Class, Type, TypeParam, Def = Value
  }

  /** The parse starting point depends on whether the source file is self-contained:
   *  if not, the AST will be supplemented.
   */
  def parser(source: SourceFile)(implicit ctx: Context) =
    if (source.isSelfContained) new ScriptParser(source)
    else new Parser(source)

  abstract class ParserCommon(val source: SourceFile)(implicit ctx: Context) extends DotClass {

    val in: ScannerCommon

    /* ------------- POSITIONS ------------------------------------------- */

    def atPos[T <: Positioned](start: Offset, point: Offset, end: Offset)(t: T): T =
      atPos(Position(start, end, point))(t)

    def atPos[T <: Positioned](start: Offset, point: Offset)(t: T): T =
      atPos(start, point, in.lastOffset max start)(t)

    def atPos[T <: Positioned](start: Offset)(t: T): T =
      atPos(start, start)(t)

    def atPos[T <: Positioned](pos: Position)(t: T): T =
      if (t.pos.isSourceDerived) t else t.withPos(pos)

    def tokenRange = Position(in.offset, in.lastCharOffset, in.offset)

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
    def syntaxError(msg: String, offset: Int = in.offset): Unit =
      if (offset > lastErrorOffset) {
        syntaxError(msg, Position(offset))
        lastErrorOffset = in.offset
      }

    /** Unconditionally issue an error at given position, without
      *  updating lastErrorOffset.
      */
    def syntaxError(msg: String, pos: Position): Unit =
      ctx.error(msg, source atPos pos)

  }

  class Parser(source: SourceFile)(implicit ctx: Context) extends ParserCommon(source) {

    val in: Scanner = new Scanner(source)

    val openParens = new ParensCounters

    /** This is the general parse entry point.
     *  Overridden by ScriptParser
     */
    def parse(): Tree = {
      val t = compilationUnit()
      accept(EOF)
      t
    }

/* -------------- TOKEN CLASSES ------------------------------------------- */

    def isIdent = in.token == IDENTIFIER || in.token == BACKQUOTED_IDENT
    def isIdent(name: Name) = in.token == IDENTIFIER && in.name == name
    def isSimpleLiteral = simpleLiteralTokens contains in.token
    def isLiteral = literalTokens contains in.token
    def isNumericLit = numericLitTokens contains in.token
    def isModifier = modifierTokens contains in.token
    def isExprIntro = canStartExpressionTokens contains in.token
    def isTemplateIntro = templateIntroTokens contains in.token
    def isDclIntro = dclIntroTokens contains in.token
    def isStatSeqEnd = in.token == RBRACE || in.token == EOF
    def mustStartStat = mustStartStatTokens contains in.token

    def isDefIntro(allowedMods: BitSet) =
      in.token == AT || (allowedMods contains in.token) || (defIntroTokens contains in.token)

    def isStatSep: Boolean =
      in.token == NEWLINE || in.token == NEWLINES || in.token == SEMI

/* ------------- ERROR HANDLING ------------------------------------------- */

    /** The offset of the last time when a statement on a new line was definitely
     *  encountered in the current scope or an outer scope\
     */
    private var lastStatOffset = -1

    def setLastStatOffset() =
      if (mustStartStat && in.isAfterLineEnd)
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

    def warning(msg: String, offset: Int = in.offset) =
      ctx.warning(msg, source atPos Position(offset))

    def deprecationWarning(msg: String, offset: Int = in.offset) =
      ctx.deprecationWarning(msg, source atPos Position(offset))

    /** Issue an error at current offset taht input is incomplete */
    def incompleteInputError(msg: String) =
      ctx.incompleteInputError(msg, source atPos Position(in.offset))

    /** If at end of file, issue an incompleteInputError.
     *  Otherwise issue a syntax error and skip to next safe point.
     */
    def syntaxErrorOrIncomplete(msg: String) =
      if (in.token == EOF) incompleteInputError(msg)
      else {
        syntaxError(msg)
        skip()
        lastErrorOffset = in.offset
      } // DEBUG

    private def expectedMsg(token: Int): String =
      expectedMessage(showToken(token))
    private def expectedMessage(what: String): String =
      s"$what expected but ${showToken(in.token)} found"

    /** Consume one token of the specified type, or
      * signal an error if it is not there.
      *
      * @return The offset at the start of the token to accept
      */
    def accept(token: Int): Int = {
      val offset = in.offset
      if (in.token != token) {
        syntaxErrorOrIncomplete(expectedMsg(token))
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

    def acceptStatSepUnlessAtEnd(altEnd: Token = EOF) =
      if (!isStatSeqEnd && in.token != altEnd) acceptStatSep()

    def errorTermTree    = atPos(in.offset) { Literal(Constant(null)) }

    private var inFunReturnType = false
    private def fromWithinReturnType[T](body: => T): T = {
      val saved = inFunReturnType
      try {
        inFunReturnType = true
        body
      } finally inFunReturnType = saved
    }

    private val isScala2Mode =
      ctx.settings.language.value.contains(nme.Scala2.toString)

    def migrationWarningOrError(msg: String, offset: Int = in.offset) =
      if (isScala2Mode)
        ctx.migrationWarning(msg, source atPos Position(offset))
      else
        syntaxError(msg, offset)

    /** Cannot use ctx.featureEnabled because accessing the context would force too much */
    private def testScala2Mode(msg: String, pos: Position = Position(in.offset)) = {
      if (isScala2Mode) ctx.migrationWarning(msg, source atPos pos)
      isScala2Mode
    }

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
        syntaxError(s"not a legal $expected (${tree.getClass})", tree.pos)
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
        syntaxError("identifier expected", tree.pos)
        tree
    }

    def emptyConstructor() = atPos(in.offset) { ast.untpd.emptyConstructor }

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
          case vd :: _ => syntaxError("unbound placeholder parameter", vd.pos)
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
    lazy val xmlp = new MarkupParser(this, true)

    object symbXMLBuilder extends SymbolicXMLBuilder(this, true) // DEBUG choices

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

    def checkAssoc(offset: Int, op: Name, leftAssoc: Boolean) =
      if (isLeftAssoc(op) != leftAssoc)
        syntaxError(
          "left- and right-associative operators with same precedence may not be mixed", offset)

    def reduceStack(base: List[OpInfo], top: Tree, prec: Int, leftAssoc: Boolean): Tree = {
      if (opStack != base && precedence(opStack.head.operator) == prec)
        checkAssoc(opStack.head.offset, opStack.head.operator, leftAssoc)
      def recur(top: Tree): Tree = {
        if (opStack == base) top
        else {
          val opInfo = opStack.head
          val opPrec = precedence(opInfo.operator)
          if (prec < opPrec || leftAssoc && prec == opPrec) {
            opStack = opStack.tail
            recur {
              val opPos = Position(opInfo.offset, opInfo.offset + opInfo.operator.length, opInfo.offset)
              atPos(opPos union opInfo.operand.pos union top.pos) {
                InfixOp(opInfo.operand, opInfo.operator, top)
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
        notAnOperator: Name = nme.EMPTY,
        maybePostfix: Boolean = false): Tree = {
      val base = opStack
      var top = first
      while (isIdent && in.name != notAnOperator) {
        val op = if (isType) in.name.toTypeName else in.name
        top = reduceStack(base, top, precedence(op), isLeftAssoc(op))
        opStack = OpInfo(top, op, in.offset) :: opStack
        ident()
        newLineOptWhenFollowing(canStartOperand)
        if (maybePostfix && !canStartOperand(in.token)) {
          val topInfo = opStack.head
          opStack = opStack.tail
          val od = reduceStack(base, topInfo.operand, 0, true)
          return atPos(od.pos.start, topInfo.offset) {
            PostfixOp(od, topInfo.operator)
          }
        }
        top = operand()
      }
      reduceStack(base, top, 0, true)
    }

/* -------- IDENTIFIERS AND LITERALS ------------------------------------------- */

    /** Accept identifier and return its name as a term name. */
    def ident(): TermName =
      if (isIdent) {
        val name = in.name
        in.nextToken()
        name
      } else {
        syntaxErrorOrIncomplete(expectedMsg(IDENTIFIER))
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
      atPos(t.pos.start, in.offset) { Select(t, ident()) }

    /** Selectors ::= ident { `.' ident()
     *
     *  Accept `.' separated identifiers acting as a selectors on given tree `t`.
     *  @param finish   An alternative parse in case the next token is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
    def selectors(t: Tree, finish: Tree => Tree): Tree = {
      val t1 = finish(t)
      if (t1 ne t) t1 else dotSelectors(selector(t), finish)
    }

    /** Dotelectors ::= { `.' ident()
     *
     *  Accept `.' separated identifiers acting as a selectors on given tree `t`.
     *  @param finish   An alternative parse in case the token following a `.' is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
     def dotSelectors(t: Tree, finish: Tree => Tree = id) =
      if (in.token == DOT) { in.nextToken(); selectors(t, finish) }
      else t

    private val id: Tree => Tree = x => x

    /** Path       ::= StableId
     *              |  [Ident `.'] this
     *
     *  @param thisOK   If true, [Ident `.'] this is acceptable as the path.
     *                  If false, another selection is required aftre the `this`.
     *  @param finish   An alternative parse in case the token following a `.' is not an identifier.
     *                  If the alternative does not apply, its tree argument is returned unchanged.
     */
    def path(thisOK: Boolean, finish: Tree => Tree = id): Tree = {
      val start = in.offset
      def handleThis(name: TypeName) = {
        in.nextToken()
        val t = atPos(start) { This(name) }
        if (!thisOK && in.token != DOT) syntaxError("`.' expected")
        dotSelectors(t, finish)
      }
      def handleSuper(name: TypeName) = {
        in.nextToken()
        val mix = mixinQualifierOpt()
        val t = atPos(start) { Super(This(name), mix) }
        accept(DOT)
        dotSelectors(selector(t), finish)
      }
      if (in.token == THIS) handleThis(tpnme.EMPTY)
      else if (in.token == SUPER) handleSuper(tpnme.EMPTY)
      else {
        val t = termIdent()
        if (in.token == DOT) {
          in.nextToken()
          if (in.token == THIS) handleThis(t.name.toTypeName)
          else if (in.token == SUPER) handleSuper(t.name.toTypeName)
          else selectors(t, finish)
        }
        else t
      }
    }

    /** MixinQualifier ::= `[' Id `]'
    */
    def mixinQualifierOpt(): TypeName =
      if (in.token == LBRACKET) inBrackets(ident().toTypeName)
      else tpnme.EMPTY

    /** StableId ::= Id
     *            |  Path `.' Id
     *            |  [id '.'] super [`[' id `]']`.' id
     */
    def stableId(): Tree =
      path(thisOK = false)

    /** QualId ::= Id {`.' Id}
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
        else if (in.token == INTERPOLATIONID) interpolatedString()
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
            syntaxErrorOrIncomplete("illegal literal")
            null
        })
      }
    }

    private def interpolatedString(inPattern: Boolean = false): Tree = atPos(in.offset) {
      val partsBuf = new ListBuffer[Literal]
      val exprBuf = new ListBuffer[Tree]
      val interpolator = in.name
      in.nextToken()
      while (in.token == STRINGPART) {
        partsBuf += literal().asInstanceOf[Literal]
        exprBuf += atPos(in.offset) {
          if (in.token == IDENTIFIER)
            termIdent()
          else if (in.token == THIS) {
            in.nextToken()
            This(tpnme.EMPTY)
          }
          else if (in.token == LBRACE)
            if (inPattern) Block(Nil, inBraces(pattern()))
            else expr()
          else {
            syntaxErrorOrIncomplete("error in interpolated string: identifier or block expected")
            EmptyTree
          }
        }
      }
      if (in.token == STRINGLIT) partsBuf += literal().asInstanceOf[Literal]
      InterpolatedString(interpolator, partsBuf.toList, exprBuf.toList)
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

    /** Type        ::= FunArgTypes `=>' Type
     *                |  HkTypeParamClause `->' Type
     *                | InfixType
     *  FunArgTypes ::=  InfixType
     *                | `(' [ FunArgType {`,' FunArgType } ] `)'
     */
    def typ(): Tree = {
      val start = in.offset
      val t =
        if (in.token == LPAREN) {
          in.nextToken()
          if (in.token == RPAREN) {
            in.nextToken()
            atPos(start, accept(ARROW)) { Function(Nil, typ()) }
          }
          else {
            openParens.change(LPAREN, 1)
            val ts = commaSeparated(funArgType)
            openParens.change(LPAREN, -1)
            accept(RPAREN)
            if (in.token == ARROW)
              atPos(start, in.skipToken()) { Function(ts, typ()) }
            else {
              for (t <- ts)
                if (t.isInstanceOf[ByNameTypeTree])
                  syntaxError("no by-name parameter type allowed here", t.pos)
              val tuple = atPos(start) { makeTupleOrParens(ts) }
              infixTypeRest(refinedTypeRest(withTypeRest(simpleTypeRest(tuple))))
            }
          }
        }
        else if (in.token == LBRACKET) {
          val tparams = typeParamClause(ParamOwner.TypeParam)
          if (isIdent && in.name.toString == "->")
            atPos(in.skipToken())(TypeLambdaTree(tparams, typ()))
          else { syntaxErrorOrIncomplete(expectedMessage("`->'")); typ() }
        }
        else infixType()

      in.token match {
        case ARROW => atPos(start, in.skipToken()) { Function(List(t), typ()) }
        case FORSOME => syntaxError("existential types no longer supported; use a wildcard type or dependent type instead"); t
        case _ => t
      }
    }

    /** InfixType ::= RefinedType {id [nl] refinedType}
     */
    def infixType(): Tree = infixTypeRest(refinedType())

    def infixTypeRest(t: Tree): Tree =
      infixOps(t, canStartTypeTokens, refinedType, isType = true, notAnOperator = nme.raw.STAR)

    /** RefinedType        ::=  WithType {Annotation | [nl] Refinement}
     */
    val refinedType: () => Tree = () => refinedTypeRest(withType())

    def refinedTypeRest(t: Tree): Tree = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) refinedTypeRest(atPos(t.pos.start) { RefinedTypeTree(t, refinement()) })
      else t
    }

    /** WithType ::= AnnotType {`with' AnnotType}    (deprecated)
     */
    def withType(): Tree = withTypeRest(annotType())

    def withTypeRest(t: Tree): Tree =
      if (in.token == WITH) {
        deprecationWarning("`with' as a type operator has been deprecated; use `&' instead")
        in.nextToken()
        AndTypeTree(t, withType())
      }
      else t

    /** AnnotType ::= SimpleType {Annotation}
     */
    def annotType(): Tree = annotTypeRest(simpleType())

    def annotTypeRest(t: Tree): Tree =
      if (in.token == AT) annotTypeRest(atPos(t.pos.start) { Annotated(annot(), t) })
      else t

    /** SimpleType       ::=  SimpleType TypeArgs
     *                     |  SimpleType `#' Id
     *                     |  StableId
     *                     |  Path `.' type
     *                     |  `(' ArgTypes `)'
     *                     |  Refinement
     *                     |  Literal
     */
    def simpleType(): Tree = simpleTypeRest {
      if (in.token == LPAREN)
        atPos(in.offset) { makeTupleOrParens(inParens(argTypes())) }
      else if (in.token == LBRACE)
        atPos(in.offset) { RefinedTypeTree(EmptyTree, refinement()) }
      else if (isSimpleLiteral) { SingletonTypeTree(literal()) }
      else path(thisOK = false, handleSingletonType) match {
        case r @ SingletonTypeTree(_) => r
        case r => convertToTypeId(r)
      }
    }

    val handleSingletonType: Tree => Tree = t =>
      if (in.token == TYPE) {
        in.nextToken()
        atPos(t.pos.start) { SingletonTypeTree(t) }
      } else t

    private def simpleTypeRest(t: Tree): Tree = in.token match {
      case HASH => simpleTypeRest(typeProjection(t))
      case LBRACKET => simpleTypeRest(atPos(t.pos.start) { AppliedTypeTree(t, typeArgs(namedOK = true)) })
      case _ => t
    }

    private def typeProjection(t: Tree): Tree = {
      accept(HASH)
      val id = typeIdent()
      atPos(t.pos.start, id.pos.start) { SelectFromTypeTree(t, id.name) }
    }

    /** ArgType      ::=  Type |  `_' TypeBounds
     */
    val argType = () =>
      if (in.token == USCORE) {
        val start = in.skipToken()
        typeBounds().withPos(Position(start, in.offset, start))
      }
      else typ()

    /** NamedTypeArg      ::=  id `=' ArgType
     */
    val namedTypeArg = () => {
      val name = ident()
      accept(EQUALS)
      NamedArg(name.toTypeName, argType())
    }

    /**   ArgTypes          ::=  ArgType {`,' ArgType}
     *                           NamedTypeArg {`,' NamedTypeArg}
     */
    def argTypes(namedOK: Boolean = false) = {
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
            otherArgs(NamedArg(name, argType()), namedTypeArg)
          case firstArg =>
            if (in.token == EQUALS) println(s"??? $firstArg")
            otherArgs(firstArg, argType)
        }
      else commaSeparated(argType)
    }

    /** FunArgType ::=  ArgType | `=>' ArgType
     */
    val funArgType = () =>
      if (in.token == ARROW) atPos(in.skipToken()) { ByNameTypeTree(argType()) }
      else argType()

    /** ParamType ::= [`=>'] ParamValueType
     */
    def paramType(): Tree =
      if (in.token == ARROW) atPos(in.skipToken()) { ByNameTypeTree(paramValueType()) }
      else paramValueType()

    /** ParamValueType ::= Type [`*']
     */
    def paramValueType(): Tree = {
      val t = typ()
      if (isIdent(nme.raw.STAR)) {
        in.nextToken()
        atPos(t.pos.start) { PostfixOp(t, nme.raw.STAR) }
      } else t
    }

    /** TypeArgs      ::= `[' ArgType {`,' ArgType} `]'
     *  NamedTypeArgs ::= `[' NamedTypeArg {`,' NamedTypeArg} `]'
     */
    def typeArgs(namedOK: Boolean = false): List[Tree] = inBrackets(argTypes(namedOK))

    /** Refinement ::= `{' RefineStatSeq `}'
     */
    def refinement(): List[Tree] = inBraces(refineStatSeq())

    /** TypeBounds ::= [`>:' Type] [`<:' Type]
     */
    def typeBounds(): TypeBoundsTree =
      atPos(in.offset) { TypeBoundsTree(bound(SUPERTYPE), bound(SUBTYPE)) }

    private def bound(tok: Int): Tree =
      if (in.token == tok) { in.nextToken(); typ() }
      else EmptyTree

    /** TypeParamBounds   ::=  TypeBounds {`<%' Type} {`:' Type}
     */
    def typeParamBounds(pname: TypeName): Tree = {
      val t = typeBounds()
      val cbs = contextBounds(pname)
      if (cbs.isEmpty) t else atPos(t.pos.start) { ContextBounds(t, cbs) }
    }

    def contextBounds(pname: TypeName): List[Tree] = in.token match {
      case COLON =>
        atPos(in.skipToken) {
          AppliedTypeTree(typ(), Ident(pname))
        } :: contextBounds(pname)
      case VIEWBOUND =>
        deprecationWarning("view bounds `<%' are deprecated, use a context bound `:' instead")
        atPos(in.skipToken) {
          Function(Ident(pname) :: Nil, typ())
        } :: contextBounds(pname)
      case _ =>
        Nil
    }

    def typedOpt(): Tree =
      if (in.token == COLON) { in.nextToken(); typ() }
      else TypeTree()

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
        val t = atPos(in.offset) { Parens(inParens(exprInParens())) }
        if (in.token == altToken) in.nextToken()
        t
      } else {
        val t = expr()
        accept(altToken)
        t
      }
    }

    /** Expr              ::=  FunParams `=>' Expr
     *                      |  Expr1
     *  FunParams         ::=  Bindings
     *                      |  [`implicit'] Id
     *                      |  `_'
     *  ExprInParens      ::=  PostfixExpr `:' Type
     *                      |  Expr
     *  BlockResult       ::=  (FunParams | [`implicit'] Id `:' InfixType) => Block
     *                      |  Expr1
     *  Expr1             ::=  `if' `(' Expr `)' {nl} Expr [[semi] else Expr]
     *                      |  `if' Expr `then' Expr [[semi] else Expr]
     *                      |  `while' `(' Expr `)' {nl} Expr
     *                      |  `while' Expr `do' Expr
     *                      |  `do' Expr [semi] `while' Expr
     *                      |  `try' Expr Catches [`finally' Expr]
     *                      |  `try' Expr [`finally' Expr]
     *                      |  `throw' Expr
     *                      |  `return' [Expr]
     *                      |  ForExpr
     *                      |  [SimpleExpr `.'] Id `=' Expr
     *                      |  SimpleExpr1 ArgumentExprs `=' Expr
     *                      |  PostfixExpr [Ascription]
     *                      |  PostfixExpr `match' `{' CaseClauses `}'
     *  Bindings          ::= `(' [Binding {`,' Binding}] `)'
     *  Binding           ::= (Id | `_') [`:' Type]
     *  Ascription        ::= `:' CompoundType
     *                      | `:' Annotation {Annotation}
     *                      | `:' `_' `*'
     */
    val exprInParens = () => expr(Location.InParens)

    def expr(): Tree = expr(Location.ElseWhere)

    def expr(location: Location.Value): Tree = {
      val saved = placeholderParams
      placeholderParams = Nil
      val t = expr1(location)
      if (in.token == ARROW) {
        placeholderParams = saved
        closureRest(t.pos.start, location, convertToParams(t))
      }
      else if (isWildcard(t)) {
        placeholderParams = placeholderParams ::: saved
        t
      }
      else
        try
          if (placeholderParams.isEmpty) t
          else Function(placeholderParams.reverse, t)
        finally placeholderParams = saved
    }

    def expr1(location: Location.Value = Location.ElseWhere): Tree = in.token match {
      case IF =>
        atPos(in.skipToken()) {
          val cond = condExpr(THEN)
          newLinesOpt()
          val thenp = expr()
          val elsep = if (in.token == ELSE) { in.nextToken(); expr() }
                      else EmptyTree
          If(cond, thenp, elsep)
        }
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
        atPos(in.skipToken()) {
          val body = expr()
          val handler =
            if (in.token == CATCH) {
              in.nextToken()
              expr()
            } else EmptyTree
          val finalizer =
            if (handler.isEmpty || in.token == FINALLY) { accept(FINALLY); expr() }
            else EmptyTree
          ParsedTry(body, handler, finalizer)
        }
      case THROW =>
        atPos(in.skipToken()) { Throw(expr()) }
      case RETURN =>
        atPos(in.skipToken()) { Return(if (isExprIntro) expr() else EmptyTree, EmptyTree) }
      case FOR =>
        forExpr()
      case IMPLICIT =>
        implicitClosure(in.skipToken(), location)
      case _ =>
        expr1Rest(postfixExpr(), location)
    }

    def expr1Rest(t: Tree, location: Location.Value) = in.token match {
      case EQUALS =>
         t match {
           case Ident(_) | Select(_, _) | Apply(_, _) =>
             atPos(t.pos.start, in.skipToken()) { Assign(t, expr()) }
           case _ =>
             t
         }
      case COLON =>
        ascription(t, location)
      case MATCH =>
        atPos(t.pos.start, in.skipToken()) {
          inBraces(Match(t, caseClauses()))
        }
      case _ =>
        t
    }

    def ascription(t: Tree, location: Location.Value) = atPos(t.pos.start, in.skipToken()) {
      in.token match {
        case USCORE =>
          val uscoreStart = in.skipToken()
          if (isIdent(nme.raw.STAR)) {
            in.nextToken()
            if (in.token != RPAREN) syntaxError("`_*' can be used only for last argument", uscoreStart)
            Typed(t, atPos(uscoreStart) { Ident(tpnme.WILDCARD_STAR) })
          } else {
            syntaxErrorOrIncomplete("`*' expected"); t
          }
        case AT if location != Location.InPattern =>
          (t /: annotations()) ((t, annot) => Annotated(annot, t))
        case _ =>
          val tpt = typeDependingOn(location)
          if (isWildcard(t) && location != Location.InPattern) {
            val vd :: rest = placeholderParams
            placeholderParams = cpy.ValDef(vd)(tpt = tpt) :: rest
          }
          Typed(t, tpt)
      }
    }

    /** Expr         ::= implicit Id `=>' Expr
     *  BlockResult  ::= implicit Id [`:' InfixType] `=>' Block
     */
    def implicitClosure(start: Int, location: Location.Value): Tree = {
      val mods = atPos(start) { Modifiers(Implicit) }
      val id = termIdent()
      val paramExpr =
        if (location == Location.InBlock && in.token == COLON)
          atPos(id.pos.start, in.skipToken()) { Typed(id, infixType()) }
        else
          id
      closureRest(start, location, convertToParam(paramExpr, mods) :: Nil)
    }

    def closureRest(start: Int, location: Location.Value, params: List[Tree]): Tree =
      atPos(start, in.offset) {
        accept(ARROW)
        Function(params, if (location == Location.InBlock) block() else expr())
      }

    /** PostfixExpr   ::= InfixExpr [Id [nl]]
     *  InfixExpr     ::= PrefixExpr
     *                  | InfixExpr Id [nl] InfixExpr
     */
    def postfixExpr(): Tree =
      infixOps(prefixExpr(), canStartExpressionTokens, prefixExpr, maybePostfix = true)

    /** PrefixExpr   ::= [`-' | `+' | `~' | `!'] SimpleExpr
    */
    val prefixExpr = () =>
      if (isIdent && nme.raw.isUnary(in.name)) {
        val start = in.offset
        val name = ident()
        if (name == nme.raw.MINUS && isNumericLit)
          simpleExprRest(literal(start), canApply = true)
        else
          atPos(start) { PrefixOp(name, simpleExpr()) }
      }
      else simpleExpr()

    /** SimpleExpr    ::= new Template
     *                 |  BlockExpr
     *                 |  SimpleExpr1 [`_']
     *  SimpleExpr1   ::= literal
     *                 |  xmlLiteral
     *                 |  Path
     *                 |  `(' [ExprsInParens] `)'
     *                 |  SimpleExpr `.' Id
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
          val pname = ctx.freshName(nme.USCORE_PARAM_PREFIX).toTermName
          val param = ValDef(pname, TypeTree(), EmptyTree).withFlags(SyntheticTermParam)
            .withPos(Position(start))
          placeholderParams = param :: placeholderParams
          atPos(start) { Ident(pname) }
        case LPAREN =>
          atPos(in.offset) { makeTupleOrParens(inParens(exprsInParensOpt())) }
        case LBRACE =>
          canApply = false
          blockExpr()
        case NEW =>
          canApply = false
          val start = in.skipToken()
          val (impl, missingBody) = template(emptyConstructor())
          impl.parents match {
            case parent :: Nil if missingBody =>
              if (parent.isType) ensureApplied(wrapNew(parent)) else parent
            case _ =>
              New(impl)
          }
        case _ =>
          if (isLiteral) literal()
          else {
            syntaxErrorOrIncomplete("illegal start of simple expression")
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
          val tapp = atPos(t.pos.start, in.offset) { TypeApply(t, typeArgs(namedOK = true)) }
          simpleExprRest(tapp, canApply = true)
        case LPAREN | LBRACE if canApply =>
          val app = atPos(t.pos.start, in.offset) { Apply(t, argumentExprs()) }
          simpleExprRest(app, canApply = true)
        case USCORE =>
          atPos(t.pos.start, in.skipToken()) { PostfixOp(t, nme.WILDCARD) }
        case _ =>
          t
      }
    }

    /**   ExprsInParens     ::=  ExprInParens {`,' ExprInParens}
     */
    def exprsInParensOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else commaSeparated(exprInParens)

    /** ParArgumentExprs ::= `(' [ExprsInParens] `)'
     *                    |  `(' [ExprsInParens `,'] PostfixExpr `:' `_' `*' ')' \
     */
    def parArgumentExprs(): List[Tree] =
      inParens(if (in.token == RPAREN) Nil else commaSeparated(argumentExpr))

    /** ArgumentExprs ::= ParArgumentExprs
     *                 |  [nl] BlockExpr
     */
    def argumentExprs(): List[Tree] =
      if (in.token == LBRACE) blockExpr() :: Nil else parArgumentExprs()

    val argumentExpr = () => exprInParens() match {
      case a @ Assign(Ident(id), rhs) => cpy.NamedArg(a)(id, rhs)
      case e => e
    }

    /** ArgumentExprss ::= {ArgumentExprs}
     */
    def argumentExprss(fn: Tree): Tree = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LPAREN || in.token == LBRACE) argumentExprss(Apply(fn, argumentExprs()))
      else fn
    }

    /** ParArgumentExprss ::= {ParArgumentExprs}
     */
    def parArgumentExprss(fn: Tree): Tree =
      if (in.token == LPAREN) parArgumentExprss(Apply(fn, parArgumentExprs()))
      else fn

    /** BlockExpr ::= `{' (CaseClauses | Block) `}'
     */
    def blockExpr(): Tree = atPos(in.offset) {
      inDefScopeBraces {
        if (in.token == CASE) Match(EmptyTree, caseClauses())
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
        if (in.token == EQUALS) atPos(pat.pos.start, in.skipToken()) { GenAlias(pat, expr()) }
        else generatorRest(pat)
      }

    /** Generator   ::=  Pattern `<-' Expr
     */
    def generator(): Tree = generatorRest(pattern1())

    def generatorRest(pat: Tree) =
      atPos(pat.pos.start, accept(LARROW)) { GenFrom(pat, expr()) }

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
        if (!wrappedEnums) syntaxErrorOrIncomplete("`yield' or `do' expected")
        ForDo(enums, expr())
      }
    }

    /** CaseClauses ::= CaseClause {CaseClause}
    */
    def caseClauses(): List[CaseDef] = {
      val buf = new ListBuffer[CaseDef]
      buf += caseClause()
      while (in.token == CASE) buf += caseClause()
      buf.toList
    }

   /** CaseClause ::= case Pattern [Guard] `=>' Block
    */
    def caseClause(): CaseDef = atPos(in.offset) {
      accept(CASE)
      CaseDef(pattern(), guard(), atPos(accept(ARROW)) { block() })
    }

    /* -------- PATTERNS ------------------------------------------- */

    /**  Pattern           ::=  Pattern1 { `|' Pattern1 }
     */
    val pattern = () => {
      val pat = pattern1()
      if (isIdent(nme.raw.BAR))
        atPos(pat.pos.start) { Alternative(pat :: patternAlts()) }
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

    /**  Pattern2    ::=  [varid `@'] InfixPattern
     */
    val pattern2 = () => infixPattern() match {
      case p @ Ident(name) if isVarPattern(p) && in.token == AT =>
        val pos = in.skipToken()

        // compatibility for Scala2 `x @ _*` syntax
        infixPattern() match {
          case pt @ Ident(tpnme.WILDCARD_STAR) =>
            migrationWarningOrError("The syntax `x @ _*' is no longer supported; use `x : _*' instead", p.pos.start)
            atPos(p.pos.start, pos) { Typed(p, pt) }
          case p =>
            atPos(p.pos.start, pos) { Bind(name, p) }
        }
      case p @ Ident(tpnme.WILDCARD_STAR) =>
        // compatibility for Scala2 `_*` syntax
        migrationWarningOrError("The syntax `_*' is no longer supported; use `x : _*' instead", p.pos.start)
        atPos(p.pos.start) { Typed(Ident(nme.WILDCARD), p) }
      case p =>
        p
    }

    /**  InfixPattern ::= SimplePattern {Id [nl] SimplePattern}
     */
    def infixPattern(): Tree =
      infixOps(simplePattern(), canStartExpressionTokens, simplePattern, notAnOperator = nme.raw.BAR)

    /** SimplePattern    ::= PatVar
     *                    |  Literal
     *                    |  XmlPattern
     *                    |  `(' [Patterns] `)'
     *                    |  SimplePattern1 [TypeArgs] [ArgumentPatterns]
     *  SimplePattern1   ::= Path
     *                    |  `{' Block `}'
     *                    |  SimplePattern1 `.' Id
     *  PatVar           ::= Id
     *                    |  `_'
     */
    val simplePattern = () => in.token match {
      case IDENTIFIER | BACKQUOTED_IDENT | THIS =>
        path(thisOK = true) match {
          case id @ Ident(nme.raw.MINUS) if isNumericLit => literal(id.pos.start)
          case t => simplePatternRest(t)
        }
      case USCORE =>
        val wildIndent = wildcardIdent()

        // compatibility for Scala2 `x @ _*` and `_*` syntax
        // `x: _*' is parsed in `ascription'
        if (isIdent(nme.raw.STAR)) {
          in.nextToken()
          if (in.token != RPAREN) syntaxError("`_*' can be used only for last argument", wildIndent.pos)
          atPos(wildIndent.pos) { Ident(tpnme.WILDCARD_STAR) }
        } else wildIndent
      case LPAREN =>
        atPos(in.offset) { makeTupleOrParens(inParens(patternsOpt())) }
      case LBRACE =>
        dotSelectors(blockExpr())
      case XMLSTART =>
        xmlLiteralPattern()
      case _ =>
        if (isLiteral) literal()
        else {
          syntaxErrorOrIncomplete("illegal start of simple pattern")
          errorTermTree
        }
    }

    def simplePatternRest(t: Tree): Tree = {
      var p = t
      if (in.token == LBRACKET)
        p = atPos(t.pos.start, in.offset) { TypeApply(p, typeArgs()) }
      if (in.token == LPAREN)
        p = atPos(t.pos.start, in.offset) { Apply(p, argumentPatterns()) }
      p
    }

    /** Patterns          ::=  Pattern [`,' Pattern]
     */
    def patterns() = commaSeparated(pattern)

    def patternsOpt(): List[Tree] =
      if (in.token == RPAREN) Nil else patterns()


    /** ArgumentPatterns  ::=  `(' [Patterns] `)'
     *                      |  `(' [Patterns `,'] Pattern2 `:' `_' `*' ')
     */
    def argumentPatterns(): List[Tree] =
      inParens(patternsOpt)

/* -------- MODIFIERS and ANNOTATIONS ------------------------------------------- */

    private def flagOfToken(tok: Int): FlagSet = tok match {
      case ABSTRACT  => Abstract
      case FINAL     => Final
      case IMPLICIT  => ImplicitCommon
      case LAZY      => Lazy
      case OVERRIDE  => Override
      case PRIVATE   => Private
      case PROTECTED => Protected
      case SEALED    => Sealed
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
      val flag = flagOfToken(in.token)
      if (mods is flag) syntaxError("repeated modifier")
      val res = addFlag(mods, flag)
      in.nextToken()
      res
    }

    private def compatible(flags1: FlagSet, flags2: FlagSet): Boolean = (
         flags1.isEmpty
      || flags2.isEmpty
      || flags1.isTermFlags && flags2.isTermFlags
      || flags1.isTypeFlags && flags2.isTypeFlags
    )

    def addFlag(mods: Modifiers, flag: FlagSet): Modifiers = {
      def incompatible(kind: String) = {
        syntaxError(s"modifier(s) `${mods.flags}' not allowed for $kind")
        Modifiers(flag)
      }
      if (compatible(mods.flags, flag)) mods | flag
      else flag match {
        case Trait => incompatible("trait")
        case Method => incompatible("method")
        case Mutable => incompatible("variable")
        case _ =>
          syntaxError(s"illegal modifier combination: ${mods.flags} and $flag")
          mods
      }
    }

    /** AccessQualifier ::= "[" (Id | this) "]"
     */
    def accessQualifierOpt(mods: Modifiers): Modifiers =
      if (in.token == LBRACKET) {
        if ((mods is Local) || mods.hasPrivateWithin)
          syntaxError("duplicate private/protected qualifier")
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
     *  LocalModifier  ::= abstract | final | sealed | implicit | lazy
     */
    def modifiers(allowed: BitSet = modifierTokens, start: Modifiers = Modifiers()): Modifiers = {
      def loop(mods: Modifiers): Modifiers = {
        if (allowed contains in.token) {
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

    /** Wrap annotation or constructor in New(...).<init> */
    def wrapNew(tpt: Tree) = Select(New(tpt), nme.CONSTRUCTOR)

    /** Adjust start of annotation or constructor to position of preceding @ or new */
    def adjustStart(start: Offset)(tree: Tree): Tree = {
      val tree1 = tree match {
        case Apply(fn, args) => cpy.Apply(tree)(adjustStart(start)(fn), args)
        case Select(qual, name) => cpy.Select(tree)(adjustStart(start)(qual), name)
        case _ => tree
      }
      if (start < tree1.pos.start) tree1.withPos(tree1.pos.withStart(start))
      else tree1
    }

    /** Annotation        ::=  `@' SimpleType {ParArgumentExprs}
     */
    def annot() =
      adjustStart(accept(AT)) { ensureApplied(parArgumentExprss(wrapNew(simpleType()))) }

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
     *  ClsTypeParam      ::=  {Annotation} [{Modifier} type] [`+' | `-']
     *                         Id [HkTypeParamClause] TypeParamBounds
     *
     *  DefTypeParamClause::=  `[' DefTypeParam {`,' DefTypeParam} `]'
     *  DefTypeParam      ::=  {Annotation} Id [HkTypeParamClause] TypeParamBounds
     *
     *  TypTypeParamCaluse::=  `[' TypTypeParam {`,' TypTypeParam} `]'
     *  TypTypeParam      ::=  {Annotation} Id [HkTypePamClause] TypeBounds
     *
     *  HkTypeParamClause ::=  `[' HkTypeParam {`,' HkTypeParam} `]'
     *  HkTypeParam       ::=  {Annotation} ['+' | `-'] (Id | _') TypeBounds
     */
    def typeParamClause(ownerKind: ParamOwner.Value): List[TypeDef] = inBrackets {
      def typeParam(): TypeDef = {
        val isConcreteOwner = ownerKind == ParamOwner.Class || ownerKind == ParamOwner.Def
        val modStart = in.offset
        var mods = annotsAsMods()
        if (ownerKind == ParamOwner.Class) {
          mods = modifiers(start = mods)
          mods =
            atPos(modStart, in.offset) {
              if (in.token == TYPE) {
                in.nextToken()
                mods | Param | ParamAccessor
              } else {
                if (mods.hasFlags) syntaxError("`type' expected")
                mods | Param | PrivateLocal
              }
            }
        }
        else mods = atPos(modStart) (mods | Param)
        if (ownerKind != ParamOwner.Def) {
          if (isIdent(nme.raw.PLUS)) mods |= Covariant
          else if (isIdent(nme.raw.MINUS)) mods |= Contravariant
          if (mods is VarianceFlags) in.nextToken()
        }
        atPos(tokenRange) {
          val name =
            if (isConcreteOwner || in.token != USCORE) ident().toTypeName
            else {
              in.nextToken()
              ctx.freshName(nme.USCORE_PARAM_PREFIX).toTypeName
            }
          val hkparams =
            if (ownerKind == ParamOwner.TypeParam) Nil
            else typeParamClauseOpt(ParamOwner.TypeParam)
          val bounds =
            if (isConcreteOwner) typeParamBounds(name)
            else typeBounds()
          TypeDef(name, hkparams, bounds).withMods(mods)
        }
      }
      commaSeparated(typeParam)
    }

    def typeParamClauseOpt(ownerKind: ParamOwner.Value): List[TypeDef] =
      if (in.token == LBRACKET) typeParamClause(ownerKind) else Nil

    /** ClsParamClauses   ::=  {ClsParamClause} [[nl] `(' `implicit' ClsParams `)']
     *  ClsParamClause    ::=  [nl] `(' [ClsParams] ')'
     *  ClsParams         ::=  ClsParam {`' ClsParam}
     *  ClsParam          ::=  {Annotation} [{Modifier} (`val' | `var')] id `:' ParamType [`=' Expr]
     *  DefParamClauses   ::=  {DefParamClause} [[nl] `(' `implicit' DefParams `)']
     *  DefParamClause    ::=  [nl] `(' [DefParams] ')'
     *  DefParams         ::=  DefParam {`,' DefParam}
     *  DefParam          ::=  {Annotation} id `:' ParamType [`=' Expr]
    */
    def paramClauses(owner: Name, ofCaseClass: Boolean = false): List[List[ValDef]] = {
      var implicitFlag = EmptyFlags
      var firstClauseOfCaseClass = ofCaseClass
      var implicitOffset = -1 // use once
      def param(): ValDef = {
        val modStart = in.offset
        var mods = annotsAsMods()
        if (owner.isTypeName) {
          mods = modifiers(start = mods) | ParamAccessor
          mods =
            atPos(modStart, in.offset) {
              if (in.token == VAL) {
                in.nextToken()
                mods
              } else if (in.token == VAR) {
                in.nextToken()
                addFlag(mods, Mutable)
              } else {
                if (!(mods.flags &~ ParamAccessor).isEmpty) syntaxError("`val' or `var' expected")
                if (firstClauseOfCaseClass) mods else mods | PrivateLocal
              }
            }
        }
        else mods = atPos(modStart) { mods | Param }
        atPos(tokenRange) {
          val name = ident()
          val tpt =
            if (ctx.settings.YmethodInfer.value && owner.isTermName && in.token != COLON) {
              TypeTree()  // XX-METHOD-INFER
            } else {
              accept(COLON)
              if (in.token == ARROW) {
                if (owner.isTypeName && !(mods is Local))
                  syntaxError(s"${if (mods is Mutable) "`var'" else "`val'"} parameters may not be call-by-name")
                else if (!implicitFlag.isEmpty)
                  syntaxError("implicit parameters may not be call-by-name")
              }
              paramType()
            }
          val default =
            if (in.token == EQUALS) { in.nextToken(); expr() }
            else EmptyTree
          if (implicitOffset >= 0) {
            mods = mods.withPos(mods.pos.withStart(implicitOffset))
            implicitOffset = -1
          }
          ValDef(name, tpt, default).withMods(addFlag(mods, implicitFlag))
        }
      }
      def paramClause(): List[ValDef] = inParens {
        if (in.token == RPAREN) Nil
        else {
          if (in.token == IMPLICIT) {
            implicitOffset = in.skipToken()
            implicitFlag = Implicit
          }
          commaSeparated(param)
        }
      }
      def clauses(): List[List[ValDef]] = {
        newLineOptWhenFollowedBy(LPAREN)
        if (in.token == LPAREN)
          paramClause() :: {
            firstClauseOfCaseClass = false
            if (implicitFlag.isEmpty) clauses() else Nil
          }
        else Nil
      }
      val start = in.offset
      val result = clauses()
      if (owner == nme.CONSTRUCTOR && (result.isEmpty || (result.head take 1 exists (_.mods is Implicit)))) {
        in.token match {
          case LBRACKET   => syntaxError("no type parameters allowed here")
          case EOF        => incompleteInputError("auxiliary constructor needs non-implicit parameter list")
          case _          => syntaxError("auxiliary constructor needs non-implicit parameter list", start)
        }
      }
      result
    }

/* -------- DEFS ------------------------------------------- */

    /** Import  ::= import ImportExpr {`,' ImportExpr}
     */
    def importClause(): List[Tree] = {
      val offset = accept(IMPORT)
      commaSeparated(importExpr) match {
        case t :: rest =>
          // The first import should start at the position of the keyword.
          t.withPos(t.pos.withStart(offset)) :: rest
        case nil => nil
      }
    }

    /**  ImportExpr ::= StableId `.' (Id | `_' | ImportSelectors)
     */
    val importExpr = () => path(thisOK = false, handleImport) match {
      case imp: Import =>
        imp
      case sel @ Select(qual, name) =>
        val selector = atPos(sel.pos.point) { Ident(name) }
        cpy.Import(sel)(qual, selector :: Nil)
      case t =>
        accept(DOT)
        Import(t, Ident(nme.WILDCARD) :: Nil)
    }

    val handleImport = { tree: Tree =>
      if (in.token == USCORE) Import(tree, importSelector() :: Nil)
      else if (in.token == LBRACE) Import(tree, inBraces(importSelectors()))
      else tree
    }

    /** ImportSelectors ::= `{' {ImportSelector `,'} (ImportSelector | `_') `}'
     */
    def importSelectors(): List[Tree] =
      if (in.token == RBRACE) Nil
      else {
        val sel = importSelector()
        sel :: {
          if (!isWildcardArg(sel) && in.token == COMMA) {
            in.nextToken()
            importSelectors()
          }
          else Nil
        }
      }

   /** ImportSelector ::= Id [`=>' Id | `=>' `_']
     */
    def importSelector(): Tree = {
      val from = termIdentOrWildcard()
      if (from.name != nme.WILDCARD && in.token == ARROW)
        atPos(from.pos.start, in.skipToken()) {
          Pair(from, termIdentOrWildcard())
        }
      else from
    }

    def posMods(start: Int, mods: Modifiers) = atPos(start, in.skipToken())(mods)

    /** Def    ::= val PatDef
     *           | var VarDef
     *           | def DefDef
     *           | type {nl} TypeDcl
     *           | TmplDef
     *  Dcl    ::= val ValDcl
     *           | var ValDcl
     *           | def DefDcl
     *           | type {nl} TypeDcl
     */
    def defOrDcl(start: Int, mods: Modifiers): Tree = in.token match {
      case VAL =>
        patDefOrDcl(posMods(start, mods), in.getDocString(start))
      case VAR =>
        patDefOrDcl(posMods(start, addFlag(mods, Mutable)), in.getDocString(start))
      case DEF =>
        defDefOrDcl(posMods(start, mods), in.getDocString(start))
      case TYPE =>
        typeDefOrDcl(posMods(start, mods), in.getDocString(start))
      case _ =>
        tmplDef(start, mods)
    }

    /** PatDef ::= Pattern2 {`,' Pattern2} [`:' Type] `=' Expr
     *  VarDef ::= PatDef | Id {`,' Id} `:' Type `=' `_'
     *  ValDcl ::= Id {`,' Id} `:' Type
     *  VarDcl ::= Id {`,' Id} `:' Type
     */
    def patDefOrDcl(mods: Modifiers, docstring: Option[String] = None): Tree = {
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
          cpy.ValDef(id)(name, tpt, rhs).withMods(mods).setComment(docstring)
        } case _ =>
          PatDef(mods, lhs, tpt, rhs)
      }
    }

    /** DefDef ::= DefSig (`:' Type [`=' Expr] | "=" Expr)
     *           | this ParamClause ParamClauses `=' ConstrExpr
     *  DefDcl ::= DefSig `:' Type
     *  DefSig ::= id [DefTypeParamClause] ParamClauses
     */
    def defDefOrDcl(mods: Modifiers, docstring: Option[String] = None): Tree = atPos(tokenRange) {
      def scala2ProcedureSyntax(resultTypeStr: String) = {
        val toInsert =
          if (in.token == LBRACE) s"$resultTypeStr ="
          else ": Unit "  // trailing space ensures that `def f()def g()` works.
        testScala2Mode(s"Procedure syntax no longer supported; `$toInsert' should be inserted here") && {
          patch(source, Position(in.lastOffset), toInsert)
          true
        }
      }
      if (in.token == THIS) {
        in.nextToken()
        val vparamss = paramClauses(nme.CONSTRUCTOR)
        val rhs = {
          if (!(in.token == LBRACE && scala2ProcedureSyntax(""))) accept(EQUALS)
          atPos(in.offset) { constrExpr() }
        }
        makeConstructor(Nil, vparamss, rhs).withMods(mods)
      } else {
        val mods1 = addFlag(mods, Method)
        val name = ident()
        val tparams = typeParamClauseOpt(ParamOwner.Def)
        val vparamss = paramClauses(name)
        var tpt = fromWithinReturnType(typedOpt())
        val rhs =
          if (in.token == EQUALS) {
            in.nextToken()
            expr
          }
          else if (!tpt.isEmpty)
            EmptyTree
          else if (scala2ProcedureSyntax(": Unit")) {
            tpt = scalaUnit
            if (in.token == LBRACE) expr()
            else EmptyTree
          }
          else {
            if (!isExprIntro) syntaxError("missing return type", in.lastOffset)
            accept(EQUALS)
            expr()
          }
        DefDef(name, tparams, vparamss, tpt, rhs).withMods(mods1).setComment(docstring)
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

    /** TypeDef ::= type Id [TypeParamClause] `=' Type
     *  TypeDcl ::= type Id [TypeParamClause] TypeBounds
     */
    def typeDefOrDcl(mods: Modifiers, docstring: Option[String] = None): Tree = {
      newLinesOpt()
      atPos(tokenRange) {
        val name = ident().toTypeName
        val tparams = typeParamClauseOpt(ParamOwner.Type)
        in.token match {
          case EQUALS =>
            in.nextToken()
            TypeDef(name, tparams, typ()).withMods(mods).setComment(docstring)
          case SUPERTYPE | SUBTYPE | SEMI | NEWLINE | NEWLINES | COMMA | RBRACE | EOF =>
            TypeDef(name, tparams, typeBounds()).withMods(mods).setComment(docstring)
          case _ =>
            syntaxErrorOrIncomplete("`=', `>:', or `<:' expected")
            EmptyTree
        }
      }
    }

    /** TmplDef ::= ([`case'] `class' | `trait') ClassDef
     *            |  [`case'] `object' ObjectDef
     */
    def tmplDef(start: Int, mods: Modifiers): Tree = {
      val docstring = in.getDocString(start)
      in.token match {
        case TRAIT =>
          classDef(posMods(start, addFlag(mods, Trait)), docstring)
        case CLASS =>
          classDef(posMods(start, mods), docstring)
        case CASECLASS =>
          classDef(posMods(start, mods | Case), docstring)
        case OBJECT =>
          objectDef(posMods(start, mods | Module), docstring)
        case CASEOBJECT =>
          objectDef(posMods(start, mods | Case | Module), docstring)
        case _ =>
          syntaxErrorOrIncomplete("expected start of definition")
          EmptyTree
      }
    }

    /** ClassDef ::= Id [ClsTypeParamClause]
     *               [ConstrMods] ClsParamClauses TemplateOpt
     */
    def classDef(mods: Modifiers, docstring: Option[String]): TypeDef = atPos(tokenRange) {
      val name = ident().toTypeName
      val constr = atPos(in.offset) {
        val tparams = typeParamClauseOpt(ParamOwner.Class)
        val cmods = constrModsOpt()
        val vparamss = paramClauses(name, mods is Case)

        makeConstructor(tparams, vparamss).withMods(cmods)
      }
      val templ = templateOpt(constr)

      TypeDef(name, templ).withMods(mods).setComment(docstring)
    }

    /** ConstrMods        ::=  AccessModifier
     *                      |  Annotation {Annotation} (AccessModifier | `this')
     */
    def constrModsOpt(): Modifiers = {
      val mods = modifiers(accessModifierTokens, annotsAsMods())
      if (mods.hasAnnotations && !mods.hasFlags)
        if (in.token == THIS) in.nextToken()
        else syntaxError("`private', `protected', or `this' expected")
      mods
    }

    /** ObjectDef       ::= Id TemplateOpt
     */
    def objectDef(mods: Modifiers, docstring: Option[String] = None): ModuleDef = {
      val name = ident()
      val template = templateOpt(emptyConstructor())

      ModuleDef(name, template).withMods(mods).setComment(docstring)
    }

/* -------- TEMPLATES ------------------------------------------- */

    /** ConstrApp         ::=  SimpleType {ParArgumentExprs}
     */
    val constrApp = () => {
      val t = annotType()
      if (in.token == LPAREN) parArgumentExprss(wrapNew(t))
      else t
    }

    /** Template          ::=  ConstrApps [TemplateBody] | TemplateBody
     *  ConstrApps        ::=  ConstrApp {`with' ConstrApp}
     *
     *  @return  a pair consisting of the template, and a boolean which indicates
     *           whether the template misses a body (i.e. no {...} part).
     */
    def template(constr: DefDef): (Template, Boolean) = {
      newLineOptWhenFollowedBy(LBRACE)
      if (in.token == LBRACE) (templateBodyOpt(constr, Nil), false)
      else {
        val parents = tokenSeparated(WITH, constrApp)
        newLineOptWhenFollowedBy(LBRACE)
        val missingBody = in.token != LBRACE
        (templateBodyOpt(constr, parents), missingBody)
      }
    }

    /** TemplateOpt = [`extends' Template | TemplateBody]
     */
    def templateOpt(constr: DefDef): Template =
      if (in.token == EXTENDS) { in.nextToken(); template(constr)._1 }
      else {
        newLineOptWhenFollowedBy(LBRACE)
        if (in.token == LBRACE) template(constr)._1
        else Template(constr, Nil, EmptyValDef, Nil).withPos(constr.pos.toSynthetic)
      }

    /** TemplateBody ::= [nl] `{' TemplateStatSeq `}'
     */
    def templateBodyOpt(constr: DefDef, parents: List[Tree]) = atPos(constr.pos.start) {
      val (self, stats) =
        if (in.token == LBRACE) templateBody() else (EmptyValDef, Nil)
      Template(constr, parents, self, stats)
    }

    def templateBody(): (ValDef, List[Tree]) = {
      val r = inDefScopeBraces { templateStatSeq() }
      if (in.token == WITH) {
        syntaxError("early definitions are not supported; use trait parameters instead")
        in.nextToken()
        template(emptyConstructor())
      }
      r
    }

/* -------- STATSEQS ------------------------------------------- */

    /** Create a tree representing a packaging */
    def makePackaging(start: Int, pkg: Tree, stats: List[Tree]): PackageDef = pkg match {
      case x: RefTree => atPos(start, pkg.pos.point)(PackageDef(x, stats))
    }

    /** Packaging ::= package QualId [nl] `{' TopStatSeq `}'
     */
    def packaging(start: Int): Tree = {
      val pkg = qualId()
      newLineOptWhenFollowedBy(LBRACE)
      val stats = inDefScopeBraces(topStatSeq)
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
            stats += objectDef(atPos(start, in.skipToken()) { Modifiers(Package) })
          else stats += packaging(start)
        }
        else if (in.token == IMPORT)
          stats ++= importClause()
        else if (in.token == AT || isTemplateIntro || isModifier)
          stats += tmplDef(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          syntaxErrorOrIncomplete("expected class or object definition")
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
     *                     | super ArgumentExprs {ArgumentExprs}
     *                     |
     */
    def templateStatSeq(): (ValDef, List[Tree]) = checkNoEscapingPlaceholders {
      var self: ValDef = EmptyValDef
      val stats = new ListBuffer[Tree]
      if (isExprIntro) {
        val first = expr1()
        if (in.token == ARROW) {
          first match {
            case Typed(tree @ This(tpnme.EMPTY), tpt) =>
              self = makeSelfDef(nme.WILDCARD, tpt).withPos(first.pos)
            case _ =>
              val ValDef(name, tpt, _) = convertToParam(first, expected = "self type clause")
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
        else if (isDefIntro(modifierTokens))
          stats += defOrDcl(in.offset, defAnnotsMods(modifierTokens))
        else if (!isStatSep) {
          exitOnError = mustStartStat
          syntaxErrorOrIncomplete("illegal start of definition")
        }
        acceptStatSepUnlessAtEnd()
      }
      (self, if (stats.isEmpty) List(EmptyTree) else stats.toList)
    }

    /** RefineStatSeq    ::= RefineStat {semi RefineStat}
     *  RefineStat       ::= Dcl
     *                     |
     *  (in reality we admit Defs and filter them out afterwards)
     */
    def refineStatSeq(): List[Tree] = {
      val stats = new ListBuffer[Tree]
      while (!isStatSeqEnd) {
        if (isDclIntro) {
          stats += defOrDcl(in.offset, Modifiers())
        } else if (!isStatSep) {
          syntaxErrorOrIncomplete(
            "illegal start of declaration" +
            (if (inFunReturnType) " (possible cause: missing `=' in front of current method body)"
             else ""))
        }
        acceptStatSepUnlessAtEnd()
      }
      stats.toList
    }

    def localDef(start: Int, implicitFlag: FlagSet): Tree =
      defOrDcl(start, addFlag(defAnnotsMods(localModifierTokens), implicitFlag))

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
        if (in.token == IMPORT) {
          stats ++= importClause()
        }
        else if (isExprIntro) {
          val t = expr(Location.InBlock)
          stats += t
          t match {
            case _: Function => return stats.toList
            case _ =>
          }
        }
        else if (isDefIntro(localModifierTokens))
          if (in.token == IMPLICIT) {
            val start = in.skipToken()
            if (isIdent) stats += implicitClosure(start, Location.InBlock)
            else stats += localDef(start, ImplicitCommon)
          } else {
            stats += localDef(in.offset, EmptyFlags)
          }
        else if (!isStatSep && (in.token != CASE)) {
          exitOnError = mustStartStat
          val addendum = if (isModifier) " (no modifiers allowed here)" else ""
          syntaxErrorOrIncomplete("illegal start of statement" + addendum)
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
            val docstring = in.getDocString(start)
            ts += objectDef(atPos(start, in.skipToken()) { Modifiers(Package) }, docstring)
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


  class OutlineParser(source: SourceFile)(implicit ctx: Context) extends Parser(source) {

    def skipBraces[T](body: T): T = {
      accept(LBRACE)
      var openBraces = 1
      while (in.token != EOF && openBraces > 0) {
        if (in.token == XMLSTART) xmlLiteral()
        else {
          if (in.token == LBRACE) openBraces += 1
          else if (in.token == RBRACE) openBraces -= 1
          in.nextToken()
        }
      }
      body
    }

    override def blockExpr(): Tree = skipBraces(EmptyTree)

    override def templateBody() = skipBraces((EmptyValDef, List(EmptyTree)))
  }
}
