package dotc.parsing

import dotc.core.Names._
import dotc.util.{SourceFile, SourcePosition, Span}
import Tokens._

import scala.annotation.{switch, tailrec}
import scala.collection.mutable

/**
 * Cross-platform scanner for the browser compiler.
 *
 * This is a simplified version of the Scala 3 scanner that handles
 * tokenization of Scala source code.
 */
object Scanners {

  /** Offset into source character array */
  type Offset = Int

  /** An undefined offset */
  val NoOffset: Offset = -1

  /** Token data storage */
  trait TokenData {
    /** The current token */
    var token: Token = EMPTY

    /** Offset of the first character of the current token */
    var offset: Offset = 0

    /** Offset after the previous token */
    var lastOffset: Offset = 0

    /** Offset of newline before token, or -1 */
    var lineOffset: Offset = -1

    /** The name of an identifier */
    var name: SimpleName = null

    /** The string value of a literal */
    var strVal: String = null

    /** The base of a number */
    var base: Int = 0

    def copyFrom(td: TokenData): Unit = {
      token = td.token
      offset = td.offset
      lastOffset = td.lastOffset
      lineOffset = td.lineOffset
      name = td.name
      strVal = td.strVal
      base = td.base
    }

    def isNewLine: Boolean = token == NEWLINE || token == NEWLINES
    def isStatSep: Boolean = isNewLine || token == SEMI
    def isIdent: Boolean = token == IDENTIFIER || token == BACKQUOTED_IDENT
    def isNestedStart: Boolean = token == LBRACE || token == INDENT
    def isNestedEnd: Boolean = token == RBRACE || token == OUTDENT
    def isColon: Boolean = token == COLONop || token == COLONfollow || token == COLONeol
    def isAfterLineEnd: Boolean = lineOffset >= 0
    def isArrow: Boolean = token == ARROW || token == CTXARROW
  }

  /** Character classification utilities */
  object Chars {
    final val LF = '\n'
    final val FF = '\f'
    final val CR = '\r'
    final val SU = '\u001A'

    def isWhitespace(c: Char): Boolean = c == ' ' || c == '\t' || c == CR || c == LF || c == FF
    def isOperatorPart(c: Char): Boolean = (c: @switch) match {
      case '~' | '!' | '@' | '#' | '%' | '^' | '*' | '+' | '-' | '<' |
           '>' | '?' | ':' | '=' | '&' | '|' | '/' | '\\' => true
      case _ => Character.getType(c) == Character.MATH_SYMBOL.toInt ||
                Character.getType(c) == Character.OTHER_SYMBOL.toInt
    }
    def isSpecial(c: Char): Boolean = isOperatorPart(c) || c == '_'
    def isDigit(c: Char): Boolean = '0' <= c && c <= '9'
    def isHexDigit(c: Char): Boolean = isDigit(c) || ('a' <= c && c <= 'f') || ('A' <= c && c <= 'F')
    def digit2int(c: Char, base: Int): Int = {
      val d = if (isDigit(c)) c - '0'
              else if ('a' <= c && c <= 'z') c - 'a' + 10
              else if ('A' <= c && c <= 'Z') c - 'A' + 10
              else -1
      if (d < base) d else -1
    }
  }

  import Chars._

  /**
   * A scanner for Scala source code.
   */
  class Scanner(source: SourceFile) extends TokenData {

    private val buf: Array[Char] = source.content
    private val end: Int = buf.length

    /** Current character */
    private var ch: Char = 0

    /** Current position in buffer */
    private var charOffset: Int = 0

    /** Buffer for building literals and identifiers */
    private val litBuf = new StringBuilder

    /** Error messages */
    private val errors = mutable.ListBuffer[(String, Offset)]()

    /** Initialize scanner */
    nextChar()
    nextToken()

    /** Get collected errors */
    def getErrors: List[(String, Offset)] = errors.toList

    /** Report an error */
    protected def error(msg: String, off: Offset = offset): Unit = {
      errors += ((msg, off))
      token = ERROR
    }

    /** Advance to next character */
    private def nextChar(): Unit = {
      if (charOffset < end) {
        ch = buf(charOffset)
        charOffset += 1
      } else {
        ch = SU
      }
    }

    /** Look ahead without consuming */
    private def lookahead: Char =
      if (charOffset < end) buf(charOffset) else SU

    /** Look ahead n characters */
    private def lookaheadN(n: Int): Char = {
      val pos = charOffset + n - 1
      if (pos < end) buf(pos) else SU
    }

    /** Put character in literal buffer */
    private def putChar(c: Char): Unit = litBuf.append(c)

    /** Clear literal buffer */
    private def clearLitBuf(): Unit = litBuf.clear()

    /** Get string from literal buffer */
    private def getLitBuf: String = litBuf.toString

    /** Set string value from literal buffer */
    private def setStrVal(): Unit = {
      strVal = litBuf.toString
      litBuf.clear()
    }

    /** Scan the next token */
    def nextToken(): Unit = {
      lastOffset = charOffset
      lineOffset = -1

      // Skip whitespace and comments
      while (ch != SU && (isWhitespace(ch) || ch == '/' && (lookahead == '/' || lookahead == '*'))) {
        if (ch == LF) {
          lineOffset = charOffset - 1
          nextChar()  // IMPORTANT: Must advance past the newline!
        } else if (ch == '/') {
          if (lookahead == '/') skipLineComment()
          else if (lookahead == '*') skipBlockComment()
          else {
            // It's actually division, not a comment
            offset = charOffset - 1
            token = IDENTIFIER
            litBuf.clear()
            litBuf.append('/')
            name = termName("/")
            return
          }
        } else {
          nextChar()
        }
      }

      offset = charOffset - 1

      (ch: @switch) match {
        case SU => token = EOF

        case 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' |
             'K' | 'L' | 'M' | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' |
             'U' | 'V' | 'W' | 'X' | 'Y' | 'Z' |
             'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' |
             'k' | 'l' | 'm' | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' |
             'u' | 'v' | 'w' | 'x' | 'y' | 'z' |
             '$' | '_' =>
          getIdentOrKeyword()

        case '0' =>
          if (lookahead == 'x' || lookahead == 'X') {
            nextChar(); nextChar()
            getNumber(16)
          } else if (lookahead == 'b' || lookahead == 'B') {
            nextChar(); nextChar()
            getNumber(2)
          } else {
            getNumber(10)
          }

        case '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' =>
          getNumber(10)

        case '"' =>
          if (lookahead == '"' && lookaheadN(2) == '"') {
            nextChar(); nextChar(); nextChar()
            getMultiLineString()
          } else {
            nextChar()
            getString()
          }

        case '\'' =>
          nextChar()
          if (Character.isUnicodeIdentifierStart(ch)) {
            getIdentOrKeyword()
            if (token == IDENTIFIER) token = QUOTEID
          } else {
            getCharLiteral()
          }

        case '(' => nextChar(); token = LPAREN
        case ')' => nextChar(); token = RPAREN
        case '[' => nextChar(); token = LBRACKET
        case ']' => nextChar(); token = RBRACKET
        case '{' => nextChar(); token = LBRACE
        case '}' => nextChar(); token = RBRACE
        case ',' => nextChar(); token = COMMA
        case ';' => nextChar(); token = SEMI
        case '.' => nextChar(); token = DOT
        case '@' => nextChar(); token = AT
        case '#' => nextChar(); token = HASH

        case ':' =>
          nextChar()
          token = COLONop

        case '=' =>
          nextChar()
          if (ch == '>') { nextChar(); token = ARROW }
          else if (ch == '>' && lookahead == '>') { nextChar(); nextChar(); token = TLARROW }
          else token = EQUALS

        case '<' =>
          nextChar()
          if (ch == '-') { nextChar(); token = LARROW }
          else if (ch == ':') { nextChar(); token = SUBTYPE }
          else if (ch == '%') { nextChar(); token = VIEWBOUND }
          else { litBuf.clear(); litBuf.append('<'); getOperatorRestContinue() }

        case '>' =>
          nextChar()
          if (ch == ':') { nextChar(); token = SUPERTYPE }
          else { litBuf.clear(); litBuf.append('>'); getOperatorRestContinue() }

        case '?' =>
          nextChar()
          if (ch == '=' && lookahead == '>') { nextChar(); nextChar(); token = CTXARROW }
          else getOperatorRest()

        case '~' | '!' | '%' | '^' | '*' | '+' | '-' | '|' | '&' | '/' | '\\' =>
          getOperatorRest()

        case _ =>
          if (Character.isUnicodeIdentifierStart(ch)) {
            getIdentOrKeyword()
          } else if (isOperatorPart(ch)) {
            getOperatorRest()
          } else {
            error(s"illegal character '${ch.toInt}'")
            nextChar()
          }
      }
    }

    /** Scan an identifier or keyword */
    private def getIdentOrKeyword(): Unit = {
      litBuf.clear()
      while (Character.isUnicodeIdentifierPart(ch) || ch == '_') {
        putChar(ch)
        nextChar()
      }
      name = termName(getLitBuf)
      token = keywordOrIdentifier(name.toString)
    }

    /** Scan an operator (starting fresh) */
    private def getOperatorRest(): Unit = {
      litBuf.clear()
      getOperatorRestContinue()
    }

    /** Continue scanning an operator (buffer already has prefix) */
    private def getOperatorRestContinue(): Unit = {
      while (isOperatorPart(ch)) {
        putChar(ch)
        nextChar()
      }
      name = termName(getLitBuf)
      token = IDENTIFIER
    }

    /** Scan a number literal */
    private def getNumber(radix: Int): Unit = {
      litBuf.clear()
      base = radix
      var isLong = false
      var isFloat = false
      var isDouble = false

      // Integer part
      while (isDigit(ch) || (radix == 16 && isHexDigit(ch)) || ch == '_') {
        if (ch != '_') putChar(ch)
        nextChar()
      }

      // Decimal part
      if (radix == 10 && ch == '.' && isDigit(lookahead)) {
        putChar(ch)
        nextChar()
        while (isDigit(ch) || ch == '_') {
          if (ch != '_') putChar(ch)
          nextChar()
        }
        isDouble = true
      }

      // Exponent part
      if (radix == 10 && (ch == 'e' || ch == 'E')) {
        putChar(ch)
        nextChar()
        if (ch == '+' || ch == '-') {
          putChar(ch)
          nextChar()
        }
        while (isDigit(ch) || ch == '_') {
          if (ch != '_') putChar(ch)
          nextChar()
        }
        isDouble = true
      }

      // Suffix
      if (ch == 'l' || ch == 'L') {
        nextChar()
        isLong = true
      } else if (ch == 'f' || ch == 'F') {
        nextChar()
        isFloat = true
      } else if (ch == 'd' || ch == 'D') {
        nextChar()
        isDouble = true
      }

      strVal = getLitBuf
      token = if (isLong) LONGLIT
              else if (isFloat) FLOATLIT
              else if (isDouble) DOUBLELIT
              else INTLIT
    }

    /** Scan a string literal */
    private def getString(): Unit = {
      litBuf.clear()
      while (ch != '"' && ch != SU && ch != LF && ch != CR) {
        if (ch == '\\') {
          nextChar()
          ch match {
            case 'n' => putChar('\n')
            case 'r' => putChar('\r')
            case 't' => putChar('\t')
            case 'b' => putChar('\b')
            case 'f' => putChar('\f')
            case '\\' => putChar('\\')
            case '"' => putChar('"')
            case '\'' => putChar('\'')
            case 'u' => // Unicode escape
              nextChar()
              var code = 0
              for (_ <- 0 until 4) {
                code = code * 16 + digit2int(ch, 16)
                nextChar()
              }
              putChar(code.toChar)
              // Don't advance here, loop will do it
              strVal = getLitBuf
              token = STRINGLIT
              return
            case _ => putChar(ch)
          }
        } else {
          putChar(ch)
        }
        nextChar()
      }
      if (ch == '"') {
        nextChar()
        setStrVal()
        token = STRINGLIT
      } else {
        error("unclosed string literal")
      }
    }

    /** Scan a multi-line string literal */
    private def getMultiLineString(): Unit = {
      litBuf.clear()
      while (!(ch == '"' && lookahead == '"' && lookaheadN(2) == '"') && ch != SU) {
        putChar(ch)
        nextChar()
      }
      if (ch == '"') {
        nextChar(); nextChar(); nextChar()
        setStrVal()
        token = STRINGLIT
      } else {
        error("unclosed multi-line string literal")
      }
    }

    /** Scan a character literal */
    private def getCharLiteral(): Unit = {
      litBuf.clear()
      if (ch == '\\') {
        nextChar()
        ch match {
          case 'n' => putChar('\n')
          case 'r' => putChar('\r')
          case 't' => putChar('\t')
          case 'b' => putChar('\b')
          case 'f' => putChar('\f')
          case '\\' => putChar('\\')
          case '\'' => putChar('\'')
          case '"' => putChar('"')
          case _ => putChar(ch)
        }
        nextChar()
      } else {
        putChar(ch)
        nextChar()
      }
      if (ch == '\'') {
        nextChar()
        setStrVal()
        token = CHARLIT
      } else {
        error("unclosed character literal")
      }
    }

    /** Skip a line comment */
    private def skipLineComment(): Unit = {
      nextChar() // skip first /
      nextChar() // skip second /
      while (ch != LF && ch != SU) nextChar()
      if (ch == LF) nextChar()
    }

    /** Skip a block comment */
    private def skipBlockComment(): Unit = {
      nextChar() // skip /
      nextChar() // skip *
      var depth = 1
      while (depth > 0 && ch != SU) {
        if (ch == '/' && lookahead == '*') {
          nextChar(); nextChar()
          depth += 1
        } else if (ch == '*' && lookahead == '/') {
          nextChar(); nextChar()
          depth -= 1
        } else {
          nextChar()
        }
      }
    }

    /** Map identifier to keyword or IDENTIFIER */
    private def keywordOrIdentifier(s: String): Token = s match {
      case "abstract" => ABSTRACT
      case "case" => CASE
      case "catch" => CATCH
      case "class" => CLASS
      case "def" => DEF
      case "do" => DO
      case "else" => ELSE
      case "enum" => ENUM
      case "export" => EXPORT
      case "extends" => EXTENDS
      case "false" => FALSE
      case "final" => FINAL
      case "finally" => FINALLY
      case "for" => FOR
      case "given" => GIVEN
      case "if" => IF
      case "implicit" => IMPLICIT
      case "import" => IMPORT
      case "lazy" => LAZY
      case "match" => MATCH
      case "new" => NEW
      case "null" => NULL
      case "object" => OBJECT
      case "override" => OVERRIDE
      case "package" => PACKAGE
      case "private" => PRIVATE
      case "protected" => PROTECTED
      case "return" => RETURN
      case "sealed" => SEALED
      case "super" => SUPER
      case "then" => THEN
      case "this" => THIS
      case "throw" => THROW
      case "trait" => TRAIT
      case "true" => TRUE
      case "try" => TRY
      case "type" => TYPE
      case "val" => VAL
      case "var" => VAR
      case "while" => WHILE
      case "with" => WITH
      case "yield" => YIELD
      case "end" => END
      case "_" => USCORE
      case _ => IDENTIFIER
    }

    /** Get source position for error messages */
    def sourcePos(off: Offset = offset): SourcePosition =
      SourcePosition(source, Span(off, off))
  }
}

