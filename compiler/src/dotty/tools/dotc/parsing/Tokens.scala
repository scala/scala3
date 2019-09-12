package dotty.tools
package dotc
package parsing

import collection.immutable.BitSet
import core.Decorators._
import core.StdNames.nme

abstract class TokensCommon {
  def maxToken: Int

  type Token = Int
  type TokenSet = BitSet

  def tokenRange(lo: Int, hi: Int): TokenSet = BitSet(lo to hi: _*)

  def showTokenDetailed(token: Int): String = debugString(token)

  def showToken(token: Int): String = {
    val str = tokenString(token)
    if (isKeyword(token)) s"'$str'" else str
  }

  val tokenString, debugString: Array[String] = new Array[String](maxToken + 1)

  def enter(token: Int, str: String, debugStr: String = ""): Unit = {
    assert(tokenString(token) == null)
    tokenString(token) = str
    debugString(token) = if (debugStr.isEmpty) str else debugStr
  }

  /** special tokens */
  final val EMPTY = 0;             enter(EMPTY, "<empty>") // a missing token, used in lookahead
  final val ERROR = 1;             enter(ERROR, "erroneous token") // an erroneous token
  final val EOF = 2;               enter(EOF, "eof")

  /** literals */
  final val CHARLIT = 3;           enter(CHARLIT, "character literal")
  final val INTLIT = 4;            enter(INTLIT, "integer literal")
  final val DECILIT = 5;           enter(DECILIT, "number literal")  // with decimal point
  final val EXPOLIT = 6;           enter(EXPOLIT, "number literal with exponent")
  final val LONGLIT = 7;           enter(LONGLIT, "long literal")
  final val FLOATLIT = 8;          enter(FLOATLIT, "float literal")
  final val DOUBLELIT = 9;         enter(DOUBLELIT, "double literal")
  final val STRINGLIT = 10;         enter(STRINGLIT, "string literal")
  final val STRINGPART = 11;       enter(STRINGPART, "string literal", "string literal part")
  //final val INTERPOLATIONID = 12;  enter(INTERPOLATIONID, "string interpolator")
  //final val QUOTEID = 13;        enter(QUOTEID, "quoted identifier") // TODO: deprecate

  /** identifiers */
  final val IDENTIFIER = 14;       enter(IDENTIFIER, "identifier")
  //final val BACKQUOTED_IDENT = 15; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  /** alphabetic keywords */
  final val IF = 20;               enter(IF, "if")
  final val FOR = 21;              enter(FOR, "for")
  final val ELSE = 22;             enter(ELSE, "else")
  final val THIS = 23;             enter(THIS, "this")
  final val NULL = 24;             enter(NULL, "null")
  final val NEW = 25;              enter(NEW, "new")
  //final val WITH = 26;             enter(WITH, "with")
  final val SUPER = 27;            enter(SUPER, "super")
  //final val CASE = 28;             enter(CASE, "case")
  //final val CASECLASS = 29;        enter(CASECLASS, "case class")
  //final val CASEOBJECT = 30;       enter(CASEOBJECT, "case object")
  //final val VAL = 31;              enter(VAL, "val")
  final val ABSTRACT = 32;         enter(ABSTRACT, "abstract")
  final val FINAL = 33;            enter(FINAL, "final")
  final val PRIVATE = 34;          enter(PRIVATE, "private")
  final val PROTECTED = 35;        enter(PROTECTED, "protected")
  final val OVERRIDE = 36;         enter(OVERRIDE, "override")
  //final val IMPLICIT = 37;         enter(IMPLICIT, "implicit")
  //final val VAR = 38;              enter(VAR, "var")
  //final val DEF = 39;              enter(DEF, "def")
  //final val TYPE = 40;             enter(TYPE, "type")
  final val EXTENDS = 41;          enter(EXTENDS, "extends")
  final val TRUE = 42;             enter(TRUE, "true")
  final val FALSE = 43;            enter(FALSE, "false")
  //final val OBJECT = 44;           enter(OBJECT, "object")
  final val CLASS = 45;            enter(CLASS, "class")
  final val IMPORT = 46;           enter(IMPORT, "import")
  final val PACKAGE = 47;          enter(PACKAGE, "package")
  //final val YIELD = 48;            enter(YIELD, "yield")
  final val DO = 49;               enter(DO, "do")
  //final val TRAIT = 50;            enter(TRAIT, "trait")
  //final val SEALED = 51;           enter(SEALED, "sealed")
  final val THROW = 52;            enter(THROW, "throw")
  final val TRY = 53;              enter(TRY, "try")
  final val CATCH = 54;            enter(CATCH, "catch")
  final val FINALLY = 55;          enter(FINALLY, "finally")
  final val WHILE = 56;            enter(WHILE, "while")
  final val RETURN = 57;           enter(RETURN, "return")
  //final val MATCH = 58;            enter(MATCH, "match")
  //final val LAZY = 59;             enter(LAZY, "lazy")
  //final val THEN = 60;             enter(THEN, "then")
  //final val FORSOME = 61;          enter(FORSOME, "forSome") // TODO: deprecate
  //final val ENUM = 62;             enter(ENUM, "enum")
  //final val ERASED = 63;           enter(ERASED, "erased")

  /** special symbols */
  final val COMMA = 70;            enter(COMMA, "','")
  final val SEMI = 71;             enter(SEMI, "';'")
  final val DOT = 72;              enter(DOT, "'.'")
  //final val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  //final val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")

  /** special keywords */
  //final val USCORE = 73;           enter(USCORE, "_")
  final val COLON = 74;            enter(COLON, ":")
  final val EQUALS = 75;           enter(EQUALS, "=")
  //final val LARROW = 76;           enter(LARROW, "<-")
  //final val ARROW = 77;            enter(ARROW, "=>")
  //final val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  //final val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  //final val HASH = 82;             enter(HASH, "#")
  final val AT = 83;               enter(AT, "@")
  //final val VIEWBOUND = 84;        enter(VIEWBOUND, "<%")

  val keywords: TokenSet

  def isKeyword(token: Token): Boolean = keywords contains token

  /** parentheses */
  final val LPAREN = 90;           enter(LPAREN, "'('")
  final val RPAREN = 91;           enter(RPAREN, "')'")
  final val LBRACKET = 92;         enter(LBRACKET, "'['")
  final val RBRACKET = 93;         enter(RBRACKET, "']'")
  final val LBRACE = 94;           enter(LBRACE, "'{'")
  final val RBRACE = 95;           enter(RBRACE, "'}'")
  final val INDENT = 96;           enter(INDENT, "indent")
  final val OUTDENT = 97;          enter(OUTDENT, "unindent")

  final val firstParen = LPAREN
  final val lastParen = OUTDENT

  def buildKeywordArray(keywords: TokenSet): (Int, Array[Int]) = {
    def start(tok: Token) = tokenString(tok).toTermName.asSimpleName.start
    def sourceKeywords = keywords.toList.filter { (kw: Token) =>
      val ts = tokenString(kw)
      (ts != null) && !ts.contains(' ')
    }

    val lastKeywordStart = sourceKeywords.map(start).max

    val arr = Array.fill(lastKeywordStart + 1)(IDENTIFIER)
    for (kw <- sourceKeywords) arr(start(kw)) = kw
    (lastKeywordStart, arr)
  }
}

object Tokens extends TokensCommon {
  final val minToken = EMPTY
  final def maxToken: Int = XMLSTART

  final val INTERPOLATIONID = 12;  enter(INTERPOLATIONID, "string interpolator")
  final val QUOTEID = 13;          enter(QUOTEID, "quoted identifier") // TODO: deprecate

  final val BACKQUOTED_IDENT = 15; enter(BACKQUOTED_IDENT, "identifier", "backquoted ident")

  final val identifierTokens: TokenSet = BitSet(IDENTIFIER, BACKQUOTED_IDENT)

  def isIdentifier(token : Int): Boolean =
    token >= IDENTIFIER && token <= BACKQUOTED_IDENT

  /** alphabetic keywords */
  final val WITH = 26;             enter(WITH, "with")
  final val CASE = 28;             enter(CASE, "case")
  final val CASECLASS = 29;        enter(CASECLASS, "case class")
  final val CASEOBJECT = 30;       enter(CASEOBJECT, "case object")
  final val VAL = 31;              enter(VAL, "val")
  final val IMPLICIT = 37;         enter(IMPLICIT, "implicit")
  final val VAR = 38;              enter(VAR, "var")
  final val DEF = 39;              enter(DEF, "def")
  final val TYPE = 40;             enter(TYPE, "type")
  final val OBJECT = 44;           enter(OBJECT, "object")
  final val YIELD = 48;            enter(YIELD, "yield")
  final val TRAIT = 50;            enter(TRAIT, "trait")
  final val SEALED = 51;           enter(SEALED, "sealed")
  final val MATCH = 58;            enter(MATCH, "match")
  final val LAZY = 59;             enter(LAZY, "lazy")
  final val THEN = 60;             enter(THEN, "then")
  final val FORSOME = 61;          enter(FORSOME, "forSome") // TODO: deprecate
  final val ENUM = 62;             enter(ENUM, "enum")
  final val ERASED = 63;           enter(ERASED, "erased")
  final val IMPLIED = 64;          enter(IMPLIED, "delegate")
  final val GIVEN = 65;            enter(GIVEN, "given")
  final val EXPORT = 66;           enter(EXPORT, "export")
  final val MACRO = 67;            enter(MACRO, "macro") // TODO: remove

  /** special symbols */
  final val NEWLINE = 78;          enter(NEWLINE, "end of statement", "new line")
  final val NEWLINES = 79;         enter(NEWLINES, "end of statement", "new lines")
  final val COLONEOL = 88;         enter(COLONEOL, ":", ": at eol")

  /** special keywords */
  final val USCORE = 73;           enter(USCORE, "_")
  final val LARROW = 76;           enter(LARROW, "<-")
  final val ARROW = 77;            enter(ARROW, "=>")
  final val SUBTYPE = 80;          enter(SUBTYPE, "<:")
  final val SUPERTYPE = 81;        enter(SUPERTYPE, ">:")
  final val HASH = 82;             enter(HASH, "#")
  final val VIEWBOUND = 84;        enter(VIEWBOUND, "<%")
  final val TLARROW = 85;          enter(TLARROW, "=>>")

  final val QUOTE = 86;            enter(QUOTE, "'")

  /** XML mode */
  final val XMLSTART = 98;         enter(XMLSTART, "$XMLSTART$<") // TODO: deprecate

  final val alphaKeywords: TokenSet = tokenRange(IF, MACRO)
  final val symbolicKeywords: TokenSet = tokenRange(USCORE, TLARROW)
  final val keywords: TokenSet = alphaKeywords | symbolicKeywords

  final val allTokens: TokenSet = tokenRange(minToken, maxToken)

  final val simpleLiteralTokens: TokenSet =
    tokenRange(CHARLIT, STRINGLIT) | BitSet(TRUE, FALSE)
  final val literalTokens: TokenSet = simpleLiteralTokens | BitSet(INTERPOLATIONID, QUOTEID, NULL)  // TODO: drop QUOTEID when symbol literals are gone

  final val atomicExprTokens: TokenSet = literalTokens | identifierTokens | BitSet(
    USCORE, NULL, THIS, SUPER, TRUE, FALSE, RETURN, QUOTEID, XMLSTART)

  final val canStartExpressionTokens: TokenSet = atomicExprTokens | BitSet(
    LBRACE, LPAREN, INDENT, QUOTE, IF, DO, WHILE, FOR, NEW, TRY, THROW, GIVEN)

  final val canStartTypeTokens: TokenSet = literalTokens | identifierTokens | BitSet(
    THIS, SUPER, USCORE, LPAREN, AT)

  final val templateIntroTokens: TokenSet = BitSet(CLASS, TRAIT, OBJECT, ENUM, CASECLASS, CASEOBJECT)

  final val dclIntroTokens: TokenSet = BitSet(DEF, VAL, VAR, TYPE, IMPLIED, GIVEN)

  final val defIntroTokens: TokenSet = templateIntroTokens | dclIntroTokens

  final val localModifierTokens: TokenSet = BitSet(
    ABSTRACT, FINAL, SEALED, IMPLICIT, LAZY, ERASED)

  final val accessModifierTokens: TokenSet = BitSet(
    PRIVATE, PROTECTED)

  final val modifierTokens: TokenSet = localModifierTokens | accessModifierTokens | BitSet(
    OVERRIDE)

  final val modifierTokensOrCase: TokenSet = modifierTokens | BitSet(CASE)

  final val modifierFollowers = modifierTokens | defIntroTokens

  final val paramIntroTokens: TokenSet = modifierTokens | identifierTokens | BitSet(AT, VAL, VAR, IMPLICIT)

  /** Is token only legal as start of statement (eof also included)? */
  final val mustStartStatTokens: TokenSet = defIntroTokens | modifierTokens | BitSet(IMPORT, EXPORT, PACKAGE)

  final val canStartStatTokens: TokenSet = canStartExpressionTokens | mustStartStatTokens | BitSet(
    AT, CASE)

  final val canEndStatTokens: TokenSet = atomicExprTokens | BitSet(
    TYPE, RPAREN, RBRACE, RBRACKET, OUTDENT)

  /** Tokens that stop a lookahead scan search for a `<-`, `then`, or `do`.
   *  Used for disambiguating between old and new syntax.
   */
  final val stopScanTokens: BitSet = mustStartStatTokens |
    BitSet(IF, ELSE, WHILE, DO, FOR, YIELD, NEW, TRY, CATCH, FINALLY, THROW, RETURN, MATCH, SEMI, EOF)

  final val numericLitTokens: TokenSet = BitSet(INTLIT, DECILIT, EXPOLIT, LONGLIT, FLOATLIT, DOUBLELIT)

  final val statCtdTokens: BitSet = BitSet(THEN, ELSE, DO, CATCH, FINALLY, YIELD, MATCH)

  final val canStartIndentTokens: BitSet =
    statCtdTokens | BitSet(COLONEOL, EQUALS, ARROW, LARROW, WHILE, TRY, FOR)
      // `if` is excluded because it often comes after `else` which makes for awkward indentation rules

  final val scala3keywords = BitSet(ENUM, ERASED, GIVEN, IMPLIED)

  final val softModifierNames = Set(nme.inline, nme.opaque)
}
