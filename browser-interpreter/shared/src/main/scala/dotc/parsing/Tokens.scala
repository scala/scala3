package dotc.parsing

import scala.collection.immutable.BitSet

/**
 * Cross-platform token definitions for the browser compiler.
 */
object Tokens {

  type Token = Int
  type TokenSet = BitSet

  def tokenRange(lo: Int, hi: Int): TokenSet = BitSet(lo to hi *)

  inline val minToken = 0
  inline val maxToken = 100

  // Token strings for debugging
  val tokenString: Array[String] = new Array[String](maxToken + 1)
  val debugString: Array[String] = new Array[String](maxToken + 1)

  private def enter(token: Int, str: String, debug: String = ""): Unit = {
    tokenString(token) = str
    debugString(token) = if (debug.isEmpty) str else debug
  }

  // Special tokens
  inline val EMPTY = 0;             enter(EMPTY, "<empty>")
  inline val ERROR = 1;             enter(ERROR, "erroneous token")
  inline val EOF = 2;               enter(EOF, "eof")

  // Literals
  inline val CHARLIT = 3;           enter(CHARLIT, "character literal")
  inline val INTLIT = 4;            enter(INTLIT, "integer literal")
  inline val DECILIT = 5;           enter(DECILIT, "number literal")
  inline val EXPOLIT = 6;           enter(EXPOLIT, "number literal with exponent")
  inline val LONGLIT = 7;           enter(LONGLIT, "long literal")
  inline val FLOATLIT = 8;          enter(FLOATLIT, "float literal")
  inline val DOUBLELIT = 9;         enter(DOUBLELIT, "double literal")
  inline val STRINGLIT = 10;        enter(STRINGLIT, "string literal")
  inline val STRINGPART = 11;       enter(STRINGPART, "string literal part")
  inline val INTERPOLATIONID = 12;  enter(INTERPOLATIONID, "string interpolator")
  inline val QUOTEID = 13;          enter(QUOTEID, "quoted identifier")

  // Identifiers
  inline val IDENTIFIER = 14;       enter(IDENTIFIER, "identifier")
  inline val BACKQUOTED_IDENT = 15; enter(BACKQUOTED_IDENT, "backquoted ident")

  // Alphabetic keywords
  inline val IF = 20;               enter(IF, "if")
  inline val FOR = 21;              enter(FOR, "for")
  inline val ELSE = 22;             enter(ELSE, "else")
  inline val THIS = 23;             enter(THIS, "this")
  inline val NULL = 24;             enter(NULL, "null")
  inline val NEW = 25;              enter(NEW, "new")
  inline val WITH = 26;             enter(WITH, "with")
  inline val SUPER = 27;            enter(SUPER, "super")
  inline val CASE = 28;             enter(CASE, "case")
  inline val CASECLASS = 29;        enter(CASECLASS, "case class")
  inline val CASEOBJECT = 30;       enter(CASEOBJECT, "case object")
  inline val VAL = 31;              enter(VAL, "val")
  inline val ABSTRACT = 32;         enter(ABSTRACT, "abstract")
  inline val FINAL = 33;            enter(FINAL, "final")
  inline val PRIVATE = 34;          enter(PRIVATE, "private")
  inline val PROTECTED = 35;        enter(PROTECTED, "protected")
  inline val OVERRIDE = 36;         enter(OVERRIDE, "override")
  inline val IMPLICIT = 37;         enter(IMPLICIT, "implicit")
  inline val VAR = 38;              enter(VAR, "var")
  inline val DEF = 39;              enter(DEF, "def")
  inline val TYPE = 40;             enter(TYPE, "type")
  inline val EXTENDS = 41;          enter(EXTENDS, "extends")
  inline val TRUE = 42;             enter(TRUE, "true")
  inline val FALSE = 43;            enter(FALSE, "false")
  inline val OBJECT = 44;           enter(OBJECT, "object")
  inline val CLASS = 45;            enter(CLASS, "class")
  inline val IMPORT = 46;           enter(IMPORT, "import")
  inline val PACKAGE = 47;          enter(PACKAGE, "package")
  inline val YIELD = 48;            enter(YIELD, "yield")
  inline val DO = 49;               enter(DO, "do")
  inline val TRAIT = 50;            enter(TRAIT, "trait")
  inline val SEALED = 51;           enter(SEALED, "sealed")
  inline val THROW = 52;            enter(THROW, "throw")
  inline val TRY = 53;              enter(TRY, "try")
  inline val CATCH = 54;            enter(CATCH, "catch")
  inline val FINALLY = 55;          enter(FINALLY, "finally")
  inline val WHILE = 56;            enter(WHILE, "while")
  inline val RETURN = 57;           enter(RETURN, "return")
  inline val MATCH = 58;            enter(MATCH, "match")
  inline val LAZY = 59;             enter(LAZY, "lazy")
  inline val THEN = 60;             enter(THEN, "then")
  inline val FORSOME = 61;          enter(FORSOME, "forSome")
  inline val ENUM = 62;             enter(ENUM, "enum")
  inline val GIVEN = 63;            enter(GIVEN, "given")
  inline val EXPORT = 64;           enter(EXPORT, "export")
  inline val MACRO = 65;            enter(MACRO, "macro")
  inline val END = 66;              enter(END, "end")

  // Special symbols
  inline val COMMA = 70;            enter(COMMA, "','")
  inline val SEMI = 71;             enter(SEMI, "';'")
  inline val DOT = 72;              enter(DOT, "'.'")
  inline val USCORE = 73;           enter(USCORE, "_")
  inline val COLONop = 74;          enter(COLONop, ":")
  inline val EQUALS = 75;           enter(EQUALS, "=")
  inline val LARROW = 76;           enter(LARROW, "<-")
  inline val ARROW = 77;            enter(ARROW, "=>")
  inline val NEWLINE = 78;          enter(NEWLINE, "new line")
  inline val NEWLINES = 79;         enter(NEWLINES, "new lines")
  inline val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  inline val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  inline val HASH = 82;             enter(HASH, "#")
  inline val AT = 83;               enter(AT, "@")
  inline val VIEWBOUND = 84;        enter(VIEWBOUND, "<%")
  inline val TLARROW = 85;          enter(TLARROW, "=>>")
  inline val CTXARROW = 86;         enter(CTXARROW, "?=>")
  inline val QUOTE = 87;            enter(QUOTE, "'")
  inline val COLONfollow = 88;      enter(COLONfollow, ":")
  inline val COLONeol = 89;         enter(COLONeol, ": at eol")
  inline val SELFARROW = 90;        enter(SELFARROW, "=>")

  // Parentheses
  inline val LPAREN = 91;           enter(LPAREN, "'('")
  inline val RPAREN = 92;           enter(RPAREN, "')'")
  inline val LBRACKET = 93;         enter(LBRACKET, "'['")
  inline val RBRACKET = 94;         enter(RBRACKET, "']'")
  inline val LBRACE = 95;           enter(LBRACE, "'{'")
  inline val RBRACE = 96;           enter(RBRACE, "'}'")
  inline val INDENT = 97;           enter(INDENT, "indent")
  inline val OUTDENT = 98;          enter(OUTDENT, "unindent")
  inline val ENDlambda = 99;        enter(ENDlambda, "end of lambda")
  inline val XMLSTART = 100;        enter(XMLSTART, "XML start")

  // Token sets
  val identifierTokens: TokenSet = BitSet(IDENTIFIER, BACKQUOTED_IDENT)

  def isIdentifier(token: Token): Boolean =
    token >= IDENTIFIER && token <= BACKQUOTED_IDENT

  val alphaKeywords: TokenSet = tokenRange(IF, END)
  val symbolicKeywords: TokenSet = tokenRange(USCORE, CTXARROW)
  val keywords: TokenSet = alphaKeywords | symbolicKeywords

  def isKeyword(token: Token): Boolean = keywords.contains(token)

  val simpleLiteralTokens: TokenSet = tokenRange(CHARLIT, STRINGLIT) | BitSet(TRUE, FALSE)
  val literalTokens: TokenSet = simpleLiteralTokens | BitSet(INTERPOLATIONID, QUOTEID, NULL)

  val atomicExprTokens: TokenSet = literalTokens | identifierTokens | BitSet(
    USCORE, NULL, THIS, SUPER, TRUE, FALSE, RETURN, QUOTEID, XMLSTART)

  val openParensTokens: TokenSet = BitSet(LBRACE, LPAREN, LBRACKET)
  val closingParens: TokenSet = BitSet(RPAREN, RBRACKET, RBRACE)

  val canStartInfixExprTokens: TokenSet = atomicExprTokens | openParensTokens | BitSet(QUOTE, NEW)
  val canStartExprTokens3: TokenSet = canStartInfixExprTokens | BitSet(INDENT, IF, WHILE, FOR, TRY, THROW)
  val canStartExprTokens2: TokenSet = canStartExprTokens3 | BitSet(DO)

  val canStartInfixTypeTokens: TokenSet = literalTokens | identifierTokens | BitSet(THIS, SUPER, USCORE, LPAREN, LBRACE, AT)
  val canStartTypeTokens: TokenSet = canStartInfixTypeTokens | BitSet(LBRACE)

  val canStartPatternTokens: TokenSet = atomicExprTokens | openParensTokens | BitSet(USCORE, QUOTE)

  val templateIntroTokens: TokenSet = BitSet(CLASS, TRAIT, OBJECT, ENUM, CASECLASS, CASEOBJECT)
  val dclIntroTokens: TokenSet = BitSet(DEF, VAL, VAR, TYPE, GIVEN)
  val defIntroTokens: TokenSet = templateIntroTokens | dclIntroTokens

  val localModifierTokens: TokenSet = BitSet(ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY)
  val accessModifierTokens: TokenSet = BitSet(PRIVATE, PROTECTED)
  val modifierTokens: TokenSet = localModifierTokens | accessModifierTokens | BitSet(OVERRIDE)
  val modifierTokensOrCase: TokenSet = modifierTokens | BitSet(CASE)

  val mustStartStatTokens: TokenSet = defIntroTokens | modifierTokens | BitSet(IMPORT, EXPORT, PACKAGE)
  val canStartStatTokens2: TokenSet = canStartExprTokens2 | mustStartStatTokens | BitSet(AT, CASE, END)
  val canStartStatTokens3: TokenSet = canStartExprTokens3 | mustStartStatTokens | BitSet(AT, CASE, END)

  val canEndStatTokens: TokenSet = atomicExprTokens | BitSet(TYPE, GIVEN, RPAREN, RBRACE, RBRACKET, OUTDENT, ENDlambda)

  val numericLitTokens: TokenSet = BitSet(INTLIT, DECILIT, EXPOLIT, LONGLIT, FLOATLIT, DOUBLELIT)

  val statCtdTokens: TokenSet = BitSet(THEN, ELSE, DO, CATCH, FINALLY, YIELD, MATCH)
  val closingRegionTokens: TokenSet = BitSet(RBRACE, RPAREN, RBRACKET, CASE) | statCtdTokens

  val canStartIndentTokens: TokenSet = statCtdTokens | BitSet(COLONeol, WITH, EQUALS, ARROW, CTXARROW, LARROW, WHILE, TRY, FOR, IF, THROW, RETURN)

  val startParamTokens: TokenSet = modifierTokens | BitSet(VAL, VAR, AT)

  val endMarkerTokens: TokenSet = identifierTokens | BitSet(IF, WHILE, FOR, MATCH, TRY, NEW, THROW, GIVEN, VAL, THIS)

  val colonEOLPredecessors: TokenSet = BitSet(RPAREN, RBRACKET, BACKQUOTED_IDENT, THIS, SUPER, NEW)

  def showTokenDetailed(token: Int): String = debugString(token)

  def showToken(token: Int): String = {
    val str = tokenString(token)
    if (isKeyword(token) || token == COLONfollow || token == COLONeol) s"'$str'" else str
  }
}

