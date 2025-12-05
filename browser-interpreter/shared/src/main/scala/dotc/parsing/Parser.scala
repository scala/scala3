package dotc.parsing

import dotc.core._
import Names._
import Constants._
import Flags._
import dotc.ast.Trees._
import dotc.util.{SourceFile, SourcePosition, Span}
import Tokens._
import Scanners._

import scala.collection.mutable

/**
 * Cross-platform parser for the browser compiler.
 *
 * Parses Scala source code into an untyped AST.
 */
class Parser(source: SourceFile) {

  private val scanner = new Scanner(source)
  private val errors = mutable.ListBuffer[String]()
  private var lastErrorOffset: Int = -1  // Prevent duplicate errors at same position

  /** Current token */
  private def token: Token = scanner.token
  private def offset: Int = scanner.offset
  private def name: SimpleName = scanner.name
  private def strVal: String = scanner.strVal

  /** Advance to next token */
  private def nextToken(): Unit = scanner.nextToken()

  /** Accept a specific token or report error */
  private def accept(expected: Token): Unit = {
    if (token == expected) nextToken()
    else {
      val suggestion = suggestFix(expected, token)
      syntaxError(s"expected ${showToken(expected)}, found ${showToken(token)}$suggestion")
    }
  }

  /** Accept a token and return whether it was present */
  private def acceptOptional(expected: Token): Boolean = {
    if (token == expected) { nextToken(); true }
    else false
  }

  /** Report a syntax error with source context */
  private def syntaxError(msg: String): Unit = {
    if (offset == lastErrorOffset) return  // Prevent duplicate errors
    lastErrorOffset = offset

    val line = source.offsetToLine(offset) + 1
    val col = source.offsetToColumn(offset) + 1
    val lineContent = source.lineContentAt(offset)
    val pointer = " " * (col - 1) + "^"

    errors += s"$line:$col: error: $msg\n  $lineContent\n  $pointer"
  }

  /** Suggest a fix for common errors */
  private def suggestFix(expected: Token, found: Token): String = {
    (expected, found) match {
      case (RPAREN, RBRACE) => "\n  Hint: Did you forget a closing parenthesis ')'?"
      case (RBRACE, RPAREN) => "\n  Hint: Did you forget a closing brace '}'?"
      case (RBRACKET, _) => "\n  Hint: Did you forget a closing bracket ']'?"
      case (EQUALS, LARROW) => "\n  Hint: Use '=' for definitions, '<-' is for for-comprehensions"
      case (COLONop, EQUALS) => "\n  Hint: Parameter needs a type annotation before '='"
      case (SEMI, _) if found == IDENTIFIER => ""  // Common, no special hint
      case _ => ""
    }
  }

  /** Skip tokens until we find a recovery point */
  private def skip(): Unit = {
    var depth = 0
    while (token != EOF) {
      token match {
        case LBRACE | LPAREN | LBRACKET => depth += 1; nextToken()
        case RBRACE | RPAREN | RBRACKET =>
          if (depth > 0) { depth -= 1; nextToken() }
          else return  // Found matching closer
        case SEMI | NEWLINE if depth == 0 => nextToken(); return
        case _ => nextToken()
      }
    }
  }

  /** Skip to next statement boundary */
  private def skipToNextStatement(): Unit = {
    while (token != EOF && token != SEMI && token != NEWLINE &&
           token != RBRACE && !isDefIntro && !isModifier) {
      nextToken()
    }
    if (token == SEMI || token == NEWLINE) nextToken()
  }

  /** Get all errors */
  def getErrors: List[String] = errors.toList

  /** Check if there were any errors */
  def hasErrors: Boolean = errors.nonEmpty

  /** Create a span from start offset to current position */
  private def spanFrom(start: Int): Span = Span(start, offset)

  // ============= Entry Points =============

  /** Parse a compilation unit */
  def parse(): List[Tree] = {
    val stats = mutable.ListBuffer[Tree]()
    while (token != EOF) {
      val before = offset
      stats ++= topLevelStatement()
      // Safety: force progress if parser gets stuck
      if (offset == before && token != EOF) {
        nextToken()
      }
    }
    stats.toList
  }

  /** Parse top-level statements */
  private def topLevelStatement(): List[Tree] = {
    skipNewlines()
    token match {
      case PACKAGE => List(packageDef())
      case IMPORT => List(importDef())
      case _ if isModifier || isDefIntro => List(definition())
      case _ if isExprIntro => List(expr())
      case SEMI | NEWLINE => nextToken(); Nil
      case EOF => Nil
      case _ =>
        syntaxError(s"expected definition or expression, found ${showToken(token)}")
        skip()
        Nil
    }
  }

  // ============= Definitions =============

  /** Parse a definition */
  private def definition(): Tree = {
    val start = offset
    val mods = modifiers()

    token match {
      case VAL => valDef(mods, start)
      case VAR => varDef(mods, start)
      case DEF => defDef(mods, start)
      case TYPE => typeDef(mods, start)
      case CLASS => classDef(mods, start)
      case TRAIT => traitDef(mods, start)
      case OBJECT => objectDef(mods, start)
      case CASE => caseDefinition(mods, start)
      case ENUM => enumDef(mods, start)
      case _ =>
        syntaxError(s"expected definition, found ${showToken(token)}")
        skip()
        EmptyTree
    }
  }

  /** Parse case class or case object */
  private def caseDefinition(mods: Modifiers, start: Int): Tree = {
    accept(CASE)
    token match {
      case CLASS => classDef(mods | Case, start)
      case OBJECT => objectDef(mods | Case, start)
      case _ =>
        syntaxError(s"expected 'class' or 'object' after 'case', found ${showToken(token)}")
        skip()
        EmptyTree
    }
  }

  /** Parse modifiers */
  private def modifiers(): Modifiers = {
    var flags: FlagSet = EmptyFlags
    val annotations = mutable.ListBuffer[Tree]()

    while (isModifier) {
      val before = offset
      token match {
        case PRIVATE => flags = flags | Private; nextToken()
        case PROTECTED => flags = flags | Protected; nextToken()
        case ABSTRACT => flags = flags | Abstract; nextToken()
        case FINAL => flags = flags | Final; nextToken()
        case SEALED => flags = flags | Sealed; nextToken()
        case IMPLICIT => flags = flags | Implicit; nextToken()
        case LAZY => flags = flags | Lazy; nextToken()
        case OVERRIDE => flags = flags | Override; nextToken()
        case CASE => flags = flags | Case; nextToken()
        case AT => annotations += annotation()
        case _ => nextToken() // soft modifiers like inline, etc.
      }
      // Safety: force progress if parser gets stuck
      if (offset == before) nextToken()
    }

    Modifiers(flags, null, annotations.toList)
  }

  /** Parse an annotation */
  private def annotation(): Tree = {
    val start = offset
    accept(AT)
    val tree = simpleExpr()
    Annotation(tree).withSpan(spanFrom(start))
  }

  /** Parse val definition */
  private def valDef(mods: Modifiers, start: Int): Tree = {
    accept(VAL)
    val n = ident()
    val tpt = optType()
    accept(EQUALS)
    val rhs = expr()
    ValDef(n, tpt, rhs).withMods(mods).withSpan(spanFrom(start))
  }

  /** Parse var definition */
  private def varDef(mods: Modifiers, start: Int): Tree = {
    accept(VAR)
    val n = ident()
    val tpt = optType()
    val rhs = if (token == EQUALS) { nextToken(); expr() } else EmptyTree
    ValDef(n, tpt, rhs).withMods(mods | Mutable).withSpan(spanFrom(start))
  }

  /** Parse def definition */
  private def defDef(mods: Modifiers, start: Int): Tree = {
    accept(DEF)
    val n = ident()
    val tparams = typeParamClause()
    val vparamss = paramClauses()
    val tpt = optType()
    val rhs = if (token == EQUALS) { nextToken(); expr() } else EmptyTree

    val paramss: List[ParamClause] =
      (if (tparams.nonEmpty) List(TypeParamClause(tparams)) else Nil) ++
      vparamss.map(TermParamClause(_))

    DefDef(n, paramss, tpt, rhs).withMods(mods | Method).withSpan(spanFrom(start))
  }

  /** Parse type definition */
  private def typeDef(mods: Modifiers, start: Int): Tree = {
    accept(TYPE)
    val n = identAsTypeName()
    val tparams = typeParamClause()
    val rhs = if (token == EQUALS) { nextToken(); typeExpr() }
              else if (token == SUBTYPE || token == SUPERTYPE) typeBounds()
              else EmptyTree
    TypeDef(n, rhs).withMods(mods).withSpan(spanFrom(start))
  }

  /** Parse class definition */
  private def classDef(mods: Modifiers, start: Int): Tree = {
    accept(CLASS)
    val n = identAsTypeName()
    val tparams = typeParamClause()
    val constr = classConstr()
    val template = templateOpt()
    ClassDef(n, tparams, Template(constr, template._1, template._2, template._3))
      .withMods(mods).withSpan(spanFrom(start))
  }

  /** Parse trait definition */
  private def traitDef(mods: Modifiers, start: Int): Tree = {
    accept(TRAIT)
    val n = identAsTypeName()
    val tparams = typeParamClause()
    val template = templateOpt()
    ClassDef(n, tparams, Template(emptyConstructor(), template._1, template._2, template._3))
      .withMods(mods | Trait).withSpan(spanFrom(start))
  }

  /** Parse object definition */
  private def objectDef(mods: Modifiers, start: Int): Tree = {
    accept(OBJECT)
    val n = ident()
    val template = templateOpt()
    ModuleDef(n, Template(emptyConstructor(), template._1, template._2, template._3))
      .withMods(mods | Module).withSpan(spanFrom(start))
  }


  /** Parse enum definition */
  private def enumDef(mods: Modifiers, start: Int): Tree = {
    accept(ENUM)
    val n = identAsTypeName()
    val tparams = typeParamClause()
    val template = templateOpt()
    ClassDef(n, tparams, Template(emptyConstructor(), template._1, template._2, template._3))
      .withMods(mods | Enum).withSpan(spanFrom(start))
  }

  /** Parse class constructor parameters */
  private def classConstr(): DefDef = {
    val vparamss = if (token == LPAREN) paramClauses() else Nil
    DefDef(termName("<init>"), vparamss.map(TermParamClause(_)), EmptyTree, EmptyTree)
  }

  private def emptyConstructor(): DefDef =
    DefDef(termName("<init>"), Nil, EmptyTree, EmptyTree)

  /** Parse template (extends parents { body }) */
  private def templateOpt(): (List[Tree], ValDef, List[Tree]) = {
    val parents = if (token == EXTENDS) { nextToken(); templateParents() } else Nil
    val (self, body) = if (token == LBRACE || token == COLONeol) templateBody() else (null, Nil)
    (parents, self, body)
  }

  private def templateParents(): List[Tree] = {
    val parents = mutable.ListBuffer[Tree]()
    parents += annotatedType()
    while (token == WITH) {
      nextToken()
      parents += annotatedType()
    }
    parents.toList
  }

  private def templateBody(): (ValDef, List[Tree]) = {
    if (token == COLONeol) nextToken()
    accept(LBRACE)
    skipNewlines()

    // Check for self type
    val self: ValDef = null // Simplified: skip self type parsing

    val stats = mutable.ListBuffer[Tree]()
    while (token != RBRACE && token != EOF) {
      stats ++= blockStatement()
      skipNewlines()
    }
    accept(RBRACE)
    (self, stats.toList)
  }

  /** Parse package definition */
  private def packageDef(): Tree = {
    val start = offset
    accept(PACKAGE)
    val pid = qualId()
    skipNewlines()

    val stats = if (token == LBRACE) {
      nextToken()
      val s = mutable.ListBuffer[Tree]()
      while (token != RBRACE && token != EOF) {
        s ++= topLevelStatement()
      }
      accept(RBRACE)
      s.toList
    } else {
      val s = mutable.ListBuffer[Tree]()
      while (token != EOF) {
        s ++= topLevelStatement()
      }
      s.toList
    }

    PackageDef(pid, stats).withSpan(spanFrom(start))
  }

  /** Parse import definition */
  private def importDef(): Tree = {
    val start = offset
    accept(IMPORT)
    val expr = qualId()
    val selectors = if (token == DOT) {
      nextToken()
      importSelectors()
    } else Nil
    Import(expr, selectors).withSpan(spanFrom(start))
  }

  private def importSelectors(): List[ImportSelector] = {
    if (token == LBRACE) {
      nextToken()
      val sels = mutable.ListBuffer[ImportSelector]()
      while (token != RBRACE && token != EOF) {
        sels += importSelector()
        if (token == COMMA) nextToken()
      }
      accept(RBRACE)
      sels.toList
    } else if (token == USCORE) {
      nextToken()
      List(ImportSelector(Ident(termName("_")), EmptyTree, EmptyTree))
    } else {
      List(ImportSelector(Ident(ident()), EmptyTree, EmptyTree))
    }
  }

  private def importSelector(): ImportSelector = {
    val imported = Ident(ident())
    val renamed = if (token == ARROW) {
      nextToken()
      if (token == USCORE) { nextToken(); Ident(termName("_")) }
      else Ident(ident())
    } else EmptyTree
    ImportSelector(imported, renamed, EmptyTree)
  }

  // ============= Types =============

  /** Parse type parameters [T, U <: Bound] */
  private def typeParamClause(): List[TypeDef] = {
    if (token != LBRACKET) return Nil
    nextToken()
    val params = mutable.ListBuffer[TypeDef]()
    while (token != RBRACKET && token != EOF) {
      params += typeParam()
      if (token == COMMA) nextToken()
    }
    accept(RBRACKET)
    params.toList
  }

  private def typeParam(): TypeDef = {
    val start = offset
    val variance = if (token == IDENTIFIER && name.toString == "+") { nextToken(); Covariant }
                   else if (token == IDENTIFIER && name.toString == "-") { nextToken(); Contravariant }
                   else EmptyFlags
    val n = identAsTypeName()
    val bounds = typeBounds()
    TypeDef(n, bounds).withMods(Modifiers(variance | Param)).withSpan(spanFrom(start))
  }

  /** Parse type bounds >: lo <: hi */
  private def typeBounds(): Tree = {
    val lo = if (token == SUPERTYPE) { nextToken(); typeExpr() } else EmptyTree
    val hi = if (token == SUBTYPE) { nextToken(); typeExpr() } else EmptyTree
    if (lo.isEmpty && hi.isEmpty) EmptyTree
    else TypeBoundsTree(lo, hi)
  }

  /** Parse parameter clauses */
  private def paramClauses(): List[List[ValDef]] = {
    val clauses = mutable.ListBuffer[List[ValDef]]()
    while (token == LPAREN) {
      clauses += paramClause()
    }
    clauses.toList
  }

  private def paramClause(): List[ValDef] = {
    accept(LPAREN)
    val params = mutable.ListBuffer[ValDef]()

    if (token != RPAREN) {
      // Check for implicit/using
      val clauseMods = if (token == IMPLICIT) { nextToken(); Modifiers(Implicit) }
                       else if (token == GIVEN) { nextToken(); Modifiers(Given) }
                       else Modifiers.Empty

      params += param(clauseMods)
      while (token == COMMA) {
        nextToken()
        params += param(clauseMods)
      }
    }
    accept(RPAREN)
    params.toList
  }

  private def param(clauseMods: Modifiers): ValDef = {
    val start = offset
    val mods = modifiers()
    val n = ident()
    accept(COLONop)
    val tpt = typeExpr()
    val default = if (token == EQUALS) { nextToken(); expr() } else EmptyTree
    ValDef(n, tpt, default).withMods(Modifiers(mods.flags | clauseMods.flags | Param, mods.privateWithin, mods.annotations)).withSpan(spanFrom(start))
  }

  /** Parse optional type annotation : Type */
  private def optType(): Tree = {
    if (token == COLONop || token == COLONfollow) {
      nextToken()
      typeExpr()
    } else EmptyTree
  }

  /** Parse a type expression */
  private def typeExpr(): Tree = infixType()

  private def infixType(): Tree = {
    var t = annotatedType()
    // Only parse infix type operators (type-level operators like `|`, `&`, etc.)
    while (token == IDENTIFIER && isTypeOperator(name.toString)) {
      val op = Ident(name)
      nextToken()
      t = AppliedTypeTree(op, List(t, annotatedType()))
    }
    t
  }

  /** Check if identifier is a type-level operator */
  private def isTypeOperator(s: String): Boolean = {
    s == "|" || s == "&" || s == "with"
  }

  private def annotatedType(): Tree = {
    var t = simpleType()
    while (token == AT) {
      val annot = annotation()
      t = Annotated(t, annot)
    }
    t
  }

  private def simpleType(): Tree = {
    val start = offset
    var t: Tree = token match {
      case LPAREN =>
        nextToken()
        if (token == RPAREN) {
          nextToken()
          Ident(typeName("Unit"))
        } else {
          val types = mutable.ListBuffer[Tree]()
          types += typeExpr()
          while (token == COMMA) {
            nextToken()
            types += typeExpr()
          }
          accept(RPAREN)
          if (types.size == 1) types.head
          else Tuple(types.toList)
        }
      case IDENTIFIER | BACKQUOTED_IDENT =>
        val id = Ident(typeName(name.toString))
        nextToken()
        id
      case THIS =>
        nextToken()
        This(EmptyTypeName)
      case _ =>
        syntaxError(s"expected type, found ${showToken(token)}")
        skip()
        EmptyTree
    }

    // Handle selections and type applications
    while (token == DOT || token == LBRACKET || token == HASH) {
      if (token == DOT) {
        nextToken()
        val n = identAsTypeName()
        t = Select(t, n)
      } else if (token == LBRACKET) {
        nextToken()
        val args = mutable.ListBuffer[Tree]()
        args += typeExpr()
        while (token == COMMA) {
          nextToken()
          args += typeExpr()
        }
        accept(RBRACKET)
        t = AppliedTypeTree(t, args.toList)
      } else if (token == HASH) {
        nextToken()
        val n = identAsTypeName()
        t = Select(t, n) // Simplified: treat # like .
      }
    }

    t.withSpan(spanFrom(start))
  }

  // ============= Expressions =============

  /** Parse an expression */
  private def expr(): Tree = expr1()

  private def expr1(): Tree = {
    val start = offset
    token match {
      case IF => ifExpr(start)
      case WHILE => whileExpr(start)
      case FOR => forExpr(start)
      case TRY => tryExpr(start)
      case THROW => throwExpr(start)
      case RETURN => returnExpr(start)
      case MATCH => matchExpr(start, Ident(termName("_"))) // Partial function
      case LBRACE => blockExpr(start)
      case NEW => newExpr(start)
      case _ => postfixExpr()
    }
  }

  private def ifExpr(start: Int): Tree = {
    accept(IF)
    val cond = if (token == LPAREN) { nextToken(); val c = expr(); accept(RPAREN); c }
              else expr()
    skipNewlines()
    if (token == THEN) nextToken()
    val thenp = expr()
    skipNewlines()
    val elsep = if (token == ELSE) { nextToken(); expr() } else Literal(Constant(()))
    If(cond, thenp, elsep).withSpan(spanFrom(start))
  }

  private def whileExpr(start: Int): Tree = {
    accept(WHILE)
    val cond = if (token == LPAREN) { nextToken(); val c = expr(); accept(RPAREN); c }
              else expr()
    skipNewlines()
    if (token == DO) nextToken()
    val body = expr()
    WhileDo(cond, body).withSpan(spanFrom(start))
  }

  private def forExpr(start: Int): Tree = {
    accept(FOR)
    val enums = if (token == LPAREN) {
      nextToken()
      val e = enumerators()
      accept(RPAREN)
      e
    } else if (token == LBRACE) {
      nextToken()
      val e = enumerators()
      accept(RBRACE)
      e
    } else enumerators()

    skipNewlines()
    if (token == YIELD) {
      nextToken()
      ForYield(enums, expr()).withSpan(spanFrom(start))
    } else {
      if (token == DO) nextToken()
      ForDo(enums, expr()).withSpan(spanFrom(start))
    }
  }

  private def enumerators(): List[Tree] = {
    val enums = mutable.ListBuffer[Tree]()
    enums += enumerator()
    while (token == SEMI || token == NEWLINE) {
      nextToken()
      if (isExprIntro || token == VAL) enums += enumerator()
    }
    enums.toList
  }

  private def enumerator(): Tree = {
    val start = offset
    if (token == VAL) nextToken()
    val pat = pattern()
    if (token == LARROW) {
      nextToken()
      GenFrom(pat, expr()).withSpan(spanFrom(start))
    } else if (token == EQUALS) {
      nextToken()
      GenAlias(pat, expr()).withSpan(spanFrom(start))
    } else {
      syntaxError("expected <- or =")
      EmptyTree
    }
  }

  private def tryExpr(start: Int): Tree = {
    accept(TRY)
    val body = expr()
    skipNewlines()
    val cases = if (token == CATCH) {
      nextToken()
      if (token == LBRACE) caseClauses()
      else List(CaseDef(Ident(termName("_")), EmptyTree, expr()))
    } else Nil
    skipNewlines()
    val finalizer = if (token == FINALLY) { nextToken(); expr() } else EmptyTree
    Try(body, cases, finalizer).withSpan(spanFrom(start))
  }

  private def throwExpr(start: Int): Tree = {
    accept(THROW)
    Throw(expr()).withSpan(spanFrom(start))
  }

  private def returnExpr(start: Int): Tree = {
    accept(RETURN)
    val e = if (isExprIntro) expr() else EmptyTree
    Return(e, EmptyTree).withSpan(spanFrom(start))
  }

  private def matchExpr(start: Int, selector: Tree): Tree = {
    accept(MATCH)
    val cases = caseClauses()
    Match(selector, cases).withSpan(spanFrom(start))
  }

  private def caseClauses(): List[CaseDef] = {
    accept(LBRACE)
    skipNewlines()
    val cases = mutable.ListBuffer[CaseDef]()
    while (token == CASE) {
      cases += caseClause()
      skipNewlines()
    }
    accept(RBRACE)
    cases.toList
  }

  private def caseClause(): CaseDef = {
    val start = offset
    accept(CASE)
    val pat = pattern()
    val guard = if (token == IF) { nextToken(); expr() } else EmptyTree
    accept(ARROW)
    val body = block()
    CaseDef(pat, guard, body).withSpan(spanFrom(start)).asInstanceOf[CaseDef]
  }

  private def blockExpr(start: Int): Tree = {
    accept(LBRACE)
    val b = block()
    accept(RBRACE)
    b.withSpan(spanFrom(start))
  }

  private def newExpr(start: Int): Tree = {
    accept(NEW)
    val tpt = simpleType()
    val args = if (token == LPAREN) argumentExprs() else Nil
    Apply(Select(New(tpt), termName("<init>")), args).withSpan(spanFrom(start))
  }

  private def postfixExpr(): Tree = {
    var t = infixExpr()
    if (token == MATCH) {
      t = matchExpr(offset, t)
    }
    t
  }

  private def infixExpr(): Tree = {
    var t = prefixExpr()
    // Only parse infix operators if the identifier looks like an operator
    // (symbolic name or backquoted) and is on the same line
    while ((token == IDENTIFIER || token == BACKQUOTED_IDENT) && isOperatorIdent(name.toString)) {
      val op = Ident(name)
      val opStart = offset
      nextToken()
      val right = prefixExpr()
      t = InfixOp(t, op, right).withSpan(Span(t.span.start, right.span.end))
    }
    t
  }

  /** Check if an identifier looks like an operator */
  private def isOperatorIdent(s: String): Boolean = {
    if (s.isEmpty) false
    else {
      val first = s.head
      // Symbolic operators start with operator characters
      first match {
        case '+' | '-' | '*' | '/' | '%' | '&' | '|' | '^' | '<' | '>' |
             '=' | '!' | '~' | ':' | '#' | '@' | '\\' | '?' => true
        case _ => false
      }
    }
  }

  private def prefixExpr(): Tree = {
    if (token == IDENTIFIER && (name.toString == "-" || name.toString == "+" ||
        name.toString == "!" || name.toString == "~")) {
      val start = offset
      val op = Ident(name)
      nextToken()
      PrefixOp(op, simpleExpr()).withSpan(spanFrom(start))
    } else {
      simpleExpr()
    }
  }

  private def simpleExpr(): Tree = {
    val start = offset
    var t: Tree = token match {
      case CHARLIT =>
        val c = strVal.head
        nextToken()
        Literal(Constant(c))
      case INTLIT =>
        val v = strVal.toInt
        nextToken()
        Literal(Constant(v))
      case LONGLIT =>
        val v = strVal.dropRight(1).toLong
        nextToken()
        Literal(Constant(v))
      case FLOATLIT =>
        val v = strVal.toFloat
        nextToken()
        Literal(Constant(v))
      case DOUBLELIT =>
        val v = strVal.toDouble
        nextToken()
        Literal(Constant(v))
      case STRINGLIT =>
        val s = strVal
        nextToken()
        Literal(Constant(s))
      case TRUE =>
        nextToken()
        Literal(Constant(true))
      case FALSE =>
        nextToken()
        Literal(Constant(false))
      case NULL =>
        nextToken()
        Literal(Constant(null))
      case IDENTIFIER | BACKQUOTED_IDENT =>
        val id = Ident(name)
        nextToken()
        id
      case THIS =>
        nextToken()
        This(EmptyTypeName)
      case SUPER =>
        nextToken()
        val mix = if (token == LBRACKET) {
          nextToken()
          val m = identAsTypeName()
          accept(RBRACKET)
          m
        } else EmptyTypeName
        Super(This(EmptyTypeName), mix)
      case LPAREN =>
        nextToken()
        if (token == RPAREN) {
          nextToken()
          Literal(Constant(()))
        } else {
          val es = mutable.ListBuffer[Tree]()
          es += expr()
          while (token == COMMA) {
            nextToken()
            es += expr()
          }
          accept(RPAREN)
          if (es.size == 1) Parens(es.head)
          else Tuple(es.toList)
        }
      case LBRACE =>
        blockExpr(start)
      case NEW =>
        newExpr(start)
      case USCORE =>
        nextToken()
        Ident(termName("_"))
      case _ =>
        syntaxError(s"expected expression, found ${showToken(token)}")
        skip()
        EmptyTree
    }

    // Handle selections, applications, type applications
    while (token == DOT || token == LPAREN || token == LBRACKET) {
      if (token == DOT) {
        nextToken()
        val n = ident()
        t = Select(t, n)
      } else if (token == LPAREN) {
        val args = argumentExprs()
        t = Apply(t, args)
      } else if (token == LBRACKET) {
        nextToken()
        val targs = mutable.ListBuffer[Tree]()
        targs += typeExpr()
        while (token == COMMA) {
          nextToken()
          targs += typeExpr()
        }
        accept(RBRACKET)
        t = TypeApply(t, targs.toList)
      }
    }

    // Handle lambda arrow
    if (token == ARROW) {
      t match {
        case Parens(inner) => return functionExpr(List(inner), start)
        case Tuple(params) => return functionExpr(params, start)
        case id: Ident => return functionExpr(List(id), start)
        case _ => // Not a lambda
      }
    }

    t.withSpan(spanFrom(start))
  }

  private def functionExpr(params: List[Tree], start: Int): Tree = {
    accept(ARROW)
    val body = expr()
    val vparams = params.map {
      case id: Ident => ValDef(id.name.toTermName, EmptyTree, EmptyTree)
      case Typed(id: Ident, tpt) => ValDef(id.name.toTermName, tpt, EmptyTree)
      case t => ValDef(termName("_"), EmptyTree, EmptyTree)
    }
    Function(vparams, body).withSpan(spanFrom(start))
  }

  private def argumentExprs(): List[Tree] = {
    accept(LPAREN)
    val args = mutable.ListBuffer[Tree]()
    if (token != RPAREN) {
      args += argumentExpr()
      while (token == COMMA) {
        nextToken()
        args += argumentExpr()
      }
    }
    accept(RPAREN)
    args.toList
  }

  private def argumentExpr(): Tree = {
    // Simplified: just parse expression (proper implementation would handle named args)
    expr()
  }

  // ============= Patterns =============

  private def pattern(): Tree = pattern1()

  private def pattern1(): Tree = {
    val start = offset
    var p = simplePattern()

    // Handle alternatives
    if (token == IDENTIFIER && name.toString == "|") {
      val alts = mutable.ListBuffer[Tree](p)
      while (token == IDENTIFIER && name.toString == "|") {
        nextToken()
        alts += simplePattern()
      }
      p = Alternative(alts.toList).withSpan(spanFrom(start))
    }

    // Handle binding
    if (token == AT) {
      p match {
        case id: Ident =>
          nextToken()
          val pat = pattern()
          return Bind(id.name, pat).withSpan(spanFrom(start))
        case _ =>
      }
    }

    // Handle typed pattern
    if (token == COLONop || token == COLONfollow) {
      nextToken()
      val tpt = typeExpr()
      p = Typed(p, tpt).withSpan(spanFrom(start))
    }

    p
  }

  private def simplePattern(): Tree = {
    val start = offset
    token match {
      case USCORE =>
        nextToken()
        Ident(termName("_")).withSpan(spanFrom(start))
      case CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT | TRUE | FALSE | NULL =>
        simpleExpr()
      case IDENTIFIER | BACKQUOTED_IDENT =>
        val id = Ident(name)
        nextToken()
        if (token == LPAREN) {
          // Extractor pattern
          val args = patternArgs()
          UnApply(id, Nil, args).withSpan(spanFrom(start))
        } else {
          id.withSpan(spanFrom(start))
        }
      case LPAREN =>
        nextToken()
        if (token == RPAREN) {
          nextToken()
          Literal(Constant(())).withSpan(spanFrom(start))
        } else {
          val pats = mutable.ListBuffer[Tree]()
          pats += pattern()
          while (token == COMMA) {
            nextToken()
            pats += pattern()
          }
          accept(RPAREN)
          if (pats.size == 1) Parens(pats.head).withSpan(spanFrom(start))
          else Tuple(pats.toList).withSpan(spanFrom(start))
        }
      case _ =>
        syntaxError(s"expected pattern, found ${showToken(token)}")
        skip()
        EmptyTree
    }
  }

  private def patternArgs(): List[Tree] = {
    accept(LPAREN)
    val pats = mutable.ListBuffer[Tree]()
    if (token != RPAREN) {
      pats += pattern()
      while (token == COMMA) {
        nextToken()
        pats += pattern()
      }
    }
    accept(RPAREN)
    pats.toList
  }

  // ============= Blocks =============

  private def block(): Tree = {
    val stats = mutable.ListBuffer[Tree]()
    skipNewlines()
    while (token != RBRACE && token != CASE && token != EOF) {
      stats ++= blockStatement()
      skipNewlines()
    }
    if (stats.isEmpty) Literal(Constant(()))
    else if (stats.size == 1 && !stats.head.isInstanceOf[DefTree]) stats.head
    else Block(stats.init.toList, stats.last)
  }

  private def blockStatement(): List[Tree] = {
    token match {
      case IMPORT => List(importDef())
      case _ if isModifier || isDefIntro => List(definition())
      case _ if isExprIntro => List(expr())
      case SEMI | NEWLINE => nextToken(); Nil
      case _ =>
        syntaxError(s"expected statement, found ${showToken(token)}")
        skip()
        Nil
    }
  }

  // ============= Utilities =============

  private def ident(): TermName = {
    if (token == IDENTIFIER || token == BACKQUOTED_IDENT) {
      val n = name
      nextToken()
      n
    } else {
      syntaxError(s"expected identifier, found ${showToken(token)}")
      termName("<error>")
    }
  }

  private def identAsTypeName(): TypeName = typeName(ident().toString)

  private def qualId(): Tree = {
    var t: Tree = Ident(ident())
    while (token == DOT) {
      nextToken()
      t = Select(t, ident())
    }
    t
  }

  private def skipNewlines(): Unit = {
    while (token == NEWLINE || token == NEWLINES) nextToken()
  }

  private def isModifier: Boolean = token match {
    case PRIVATE | PROTECTED | ABSTRACT | FINAL | SEALED |
         IMPLICIT | LAZY | OVERRIDE | AT => true
    case IDENTIFIER if name.toString == "inline" || name.toString == "transparent" ||
                       name.toString == "opaque" || name.toString == "open" ||
                       name.toString == "infix" => true
    case _ => false
  }

  private def isDefIntro: Boolean = token match {
    case VAL | VAR | DEF | TYPE | CLASS | TRAIT | OBJECT | ENUM | CASE | GIVEN => true
    case _ => false
  }

  private def isExprIntro: Boolean = token match {
    case IDENTIFIER | BACKQUOTED_IDENT | USCORE |
         CHARLIT | INTLIT | LONGLIT | FLOATLIT | DOUBLELIT | STRINGLIT |
         TRUE | FALSE | NULL | THIS | SUPER | NEW |
         LPAREN | LBRACE | IF | WHILE | FOR | TRY | THROW | RETURN => true
    case _ => false
  }
}

