package dotty.tools
package dotc
package parsing

import core.Contexts._
import core.Names.SimpleName
import Scanners._
import util.SourceFile
import JavaTokens._
import scala.annotation.{ switch, tailrec }
import util.Chars._

object JavaScanners {

  class JavaScanner(source: SourceFile, override val startFrom: Offset = 0)(using Context) extends ScannerCommon(source) {

    override def decodeUni: Boolean = true

    def toToken(name: SimpleName): Token = {
      val idx = name.start
      if (idx >= 0 && idx <= lastKeywordStart) kwArray(idx) else IDENTIFIER
    }

    private class JavaTokenData0 extends TokenData

    /** we need one token lookahead
      */
    val next : TokenData = new JavaTokenData0
    val prev : TokenData = new JavaTokenData0

    // Get next token ------------------------------------------------------------

    def nextToken(): Unit =
      if (next.token == EMPTY) {
        lastOffset = lastCharOffset
        fetchToken()
      }
      else {
        this copyFrom next
        next.token = EMPTY
      }

    def lookaheadToken: Int = {
      prev copyFrom this
      nextToken()
      val t = token
      next copyFrom this
      this copyFrom prev
      t
    }

    /** read next token
      */
    private def fetchToken(): Unit = {
      offset = charOffset - 1
      ch match {
        case ' ' | '\t' | CR | LF | FF =>
          nextChar()
          fetchToken()
        case _ =>
          (ch: @switch) match {
            case 'A' | 'B' | 'C' | 'D' | 'E' |
                 'F' | 'G' | 'H' | 'I' | 'J' |
                 'K' | 'L' | 'M' | 'N' | 'O' |
                 'P' | 'Q' | 'R' | 'S' | 'T' |
                 'U' | 'V' | 'W' | 'X' | 'Y' |
                 'Z' | '$' | '_' |
                 'a' | 'b' | 'c' | 'd' | 'e' |
                 'f' | 'g' | 'h' | 'i' | 'j' |
                 'k' | 'l' | 'm' | 'n' | 'o' |
                 'p' | 'q' | 'r' | 's' | 't' |
                 'u' | 'v' | 'w' | 'x' | 'y' |
                 'z' =>
              putChar(ch)
              nextChar()
              getIdentRest()

            case '0' =>
              putChar(ch)
              nextChar()
              if (ch == 'x' || ch == 'X') {
                nextChar()
                base = 16
              }
              else
                base = 8
              getNumber()

            case '1' | '2' | '3' | '4' |
                 '5' | '6' | '7' | '8' | '9' =>
              base = 10
              getNumber()

            case '\"' =>
              nextChar()
              while (ch != '\"' && (isUnicodeEscape || ch != CR && ch != LF && ch != SU))
                getlitch()
              if (ch == '\"') {
                token = STRINGLIT
                setStrVal()
                nextChar()
              }
              else
                error("unclosed string literal")

            case '\'' =>
              nextChar()
              getlitch()
              if (ch == '\'') {
                nextChar()
                token = CHARLIT
                setStrVal()
              }
              else
                error("unclosed character literal")

            case '=' =>
              token = EQUALS
              nextChar()
              if (ch == '=') {
                token = EQEQ
                nextChar()
              }

            case '>' =>
              token = GT
              nextChar()
              if (ch == '=') {
                token = GTEQ
                nextChar()
              }
              else if (ch == '>') {
                token = GTGT
                nextChar()
                if (ch == '=') {
                  token = GTGTEQ
                  nextChar()
                }
                else if (ch == '>') {
                  token = GTGTGT
                  nextChar()
                  if (ch == '=') {
                    token = GTGTGTEQ
                    nextChar()
                  }
                }
              }

            case '<' =>
              token = LT
              nextChar()
              if (ch == '=') {
                token = LTEQ
                nextChar()
              }
              else if (ch == '<') {
                token = LTLT
                nextChar()
                if (ch == '=') {
                  token = LTLTEQ
                  nextChar()
                }
              }

            case '!' =>
              token = BANG
              nextChar()
              if (ch == '=') {
                token = BANGEQ
                nextChar()
              }

            case '~' =>
              token = TILDE
              nextChar()

            case '?' =>
              token = QMARK
              nextChar()

            case ':' =>
              token = COLONop
              nextChar()

            case '@' =>
              token = AT
              nextChar()

            case '&' =>
              token = AMP
              nextChar()
              if (ch == '&') {
                token = AMPAMP
                nextChar()
              }
              else if (ch == '=') {
                token = AMPEQ
                nextChar()
              }

            case '|' =>
              token = BAR
              nextChar()
              if (ch == '|') {
                token = BARBAR
                nextChar()
              }
              else if (ch == '=') {
                token = BAREQ
                nextChar()
              }

            case '+' =>
              token = PLUS
              nextChar()
              if (ch == '+') {
                token = PLUSPLUS
                nextChar()
              }
              else if (ch == '=') {
                token = PLUSEQ
                nextChar()
              }

            case '-' =>
              token = MINUS
              nextChar()
              if (ch == '-') {
                token = MINUSMINUS
                nextChar()
              }
              else if (ch == '=') {
                token = MINUSEQ
                nextChar()
              }

            case '*' =>
              token = ASTERISK
              nextChar()
              if (ch == '=') {
                token = ASTERISKEQ
                nextChar()
              }

            case '/' =>
              nextChar()
              if (!skipComment()) {
                token = SLASH
                nextChar()
                if (ch == '=') {
                  token = SLASHEQ
                  nextChar()
                }
              }
              else fetchToken()

            case '^' =>
              token = HAT
              nextChar()
              if (ch == '=') {
                token = HATEQ
                nextChar()
              }

            case '%' =>
              token = PERCENT
              nextChar()
              if (ch == '=') {
                token = PERCENTEQ
                nextChar()
              }

            case '.' =>
              token = DOT
              nextChar()
              if ('0' <= ch && ch <= '9') {
                putChar('.');
                getFraction()
              }
              else if (ch == '.') {
                nextChar()
                if (ch == '.') {
                  nextChar()
                  token = DOTDOTDOT
                }
                else error("`.` character expected")
              }

            case ';' =>
              token = SEMI
              nextChar()

            case ',' =>
              token = COMMA
              nextChar()

            case '(' =>
              token = LPAREN
              nextChar()

            case '{' =>
              token = LBRACE
              nextChar()

            case ')' =>
              token = RPAREN
              nextChar()

            case '}' =>
              token = RBRACE
              nextChar()

            case '[' =>
              token = LBRACKET
              nextChar()

            case ']' =>
              token = RBRACKET
              nextChar()

            case SU =>
              if (isAtEnd) token = EOF
              else {
                error("illegal character")
                nextChar()
              }

            case _ =>
              if (Character.isUnicodeIdentifierStart(ch)) {
                putChar(ch)
                nextChar()
                getIdentRest()
              }
              else {
                error("illegal character: " + ch.toInt)
                nextChar()
              }
          }
      }
    }

    protected def skipComment(): Boolean = {
      @tailrec def skipLineComment(): Unit = ch match {
        case CR | LF | SU =>
        case _ => nextChar(); skipLineComment()
      }
      @tailrec def skipJavaComment(): Unit = ch match {
        case SU => incompleteInputError("unclosed comment")
        case '*' => nextChar(); if (ch == '/') nextChar() else skipJavaComment()
        case _ => nextChar(); skipJavaComment()
      }
      ch match {
        case '/' => nextChar(); skipLineComment(); true
        case '*' => nextChar(); skipJavaComment(); true
        case _ => false
      }
    }

    // Identifiers ---------------------------------------------------------------

    private def getIdentRest(): Unit =
      while (true)
        (ch: @switch) match {
          case 'A' | 'B' | 'C' | 'D' | 'E' |
               'F' | 'G' | 'H' | 'I' | 'J' |
               'K' | 'L' | 'M' | 'N' | 'O' |
               'P' | 'Q' | 'R' | 'S' | 'T' |
               'U' | 'V' | 'W' | 'X' | 'Y' |
               'Z' | '$' |
               'a' | 'b' | 'c' | 'd' | 'e' |
               'f' | 'g' | 'h' | 'i' | 'j' |
               'k' | 'l' | 'm' | 'n' | 'o' |
               'p' | 'q' | 'r' | 's' | 't' |
               'u' | 'v' | 'w' | 'x' | 'y' |
               'z' |
               '0' | '1' | '2' | '3' | '4' |
               '5' | '6' | '7' | '8' | '9' =>
            putChar(ch)
            nextChar()

          case '_' =>
            putChar(ch)
            nextChar()
            getIdentRest()
            return
          case SU =>
            finishNamed()
            return
          case _ =>
            if (Character.isUnicodeIdentifierPart(ch)) {
              putChar(ch)
              nextChar()
            }
            else {
              finishNamed()
              return
            }
        }

    // Literals -----------------------------------------------------------------

    /** read next character in character or string literal:
      */
    protected def getlitch(): Unit =
      if (ch == '\\') {
        nextChar()
        if ('0' <= ch && ch <= '7') {
          val leadch: Char = ch
          var oct: Int = digit2int(ch, 8)
          nextChar()
          if ('0' <= ch && ch <= '7') {
            oct = oct * 8 + digit2int(ch, 8)
            nextChar()
            if (leadch <= '3' && '0' <= ch && ch <= '7') {
              oct = oct * 8 + digit2int(ch, 8)
              nextChar()
            }
          }
          putChar(oct.asInstanceOf[Char])
        }
        else {
          ch match {
            case 'b' => putChar('\b')
            case 't' => putChar('\t')
            case 'n' => putChar('\n')
            case 'f' => putChar('\f')
            case 'r' => putChar('\r')
            case '\"' => putChar('\"')
            case '\'' => putChar('\'')
            case '\\' => putChar('\\')
            case _ =>
              error("invalid escape character", charOffset - 1)
              putChar(ch)
          }
          nextChar()
        }
      }
      else {
        putChar(ch)
        nextChar()
      }

    /** read fractional part and exponent of floating point number
      * if one is present.
      */
    protected def getFraction(): Unit = {
      token = DOUBLELIT
      while ('0' <= ch && ch <= '9') {
        putChar(ch)
        nextChar()
      }
      if (ch == 'e' || ch == 'E') {
        val lookahead = lookaheadReader()
        lookahead.nextChar()
        if (lookahead.ch == '+' || lookahead.ch == '-')
          lookahead.nextChar()
        if ('0' <= lookahead.ch && lookahead.ch <= '9') {
          putChar(ch)
          nextChar()
          if (ch == '+' || ch == '-') {
            putChar(ch)
            nextChar()
          }
          while ('0' <= ch && ch <= '9') {
            putChar(ch)
            nextChar()
          }
        }
        token = DOUBLELIT
      }
      if (ch == 'd' || ch == 'D') {
        putChar(ch)
        nextChar()
        token = DOUBLELIT
      }
      else if (ch == 'f' || ch == 'F') {
        putChar(ch)
        nextChar()
        token = FLOATLIT
      }
      setStrVal()
    }

    /** convert name to long value
     */
    def intVal(negated: Boolean): Long =
      if (token == CHARLIT && !negated)
        if (strVal.length > 0) strVal.charAt(0).toLong else 0
      else {
        var value: Long = 0
        val divider = if (base == 10) 1 else 2
        val limit: Long =
          if (token == LONGLIT) Long.MaxValue else Int.MaxValue
        var i = 0
        val len = strVal.length
        while (i < len) {
          val d = digit2int(strVal.charAt(i), base)
          if (d < 0) {
            error("malformed integer number")
            return 0
          }
          if (value < 0 ||
              limit / (base / divider) < value ||
              limit - (d / divider) < value * (base / divider) &&
              !(negated && limit == value * base - 1 + d)) {
                error("integer number too large")
                return 0
              }
          value = value * base + d
          i += 1
        }
        if (negated) -value else value
      }

    /** convert name, base to double value
     */
    def floatVal(negated: Boolean): Double = {
      val limit: Double =
        if (token == DOUBLELIT) Double.MaxValue else Float.MaxValue
      try {
        val value: Double = java.lang.Double.valueOf(strVal.toString).nn.doubleValue()
        if (value > limit)
          error("floating point number too large")
        if (negated) -value else value
      } catch {
        case _: NumberFormatException =>
          error("malformed floating point number")
          0.0
      }
    }

    /** read a number into name and set base
      */
    protected def getNumber(): Unit = {
      while (digit2int(ch, if (base < 10) 10 else base) >= 0) {
        putChar(ch)
        nextChar()
      }
      token = INTLIT
      if (base <= 10 && ch == '.') {
        val lookahead = lookaheadReader()
        lookahead.nextChar()
        lookahead.ch match {
          case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' |
               '8' | '9' | 'd' | 'D' | 'e' | 'E' | 'f' | 'F' =>
            putChar(ch)
            nextChar()
            return getFraction()
          case _ =>
            if (!isIdentifierStart(lookahead.ch)) {
              putChar(ch)
              nextChar()
              return getFraction()
            }
        }
      }
      if (base <= 10 &&
        (ch == 'e' || ch == 'E' ||
          ch == 'f' || ch == 'F' ||
          ch == 'd' || ch == 'D'))
        return getFraction()
      setStrVal()
      if (ch == 'l' || ch == 'L') {
        nextChar()
        token = LONGLIT
      }
    }

    // Errors -----------------------------------------------------------------

    override def toString(): String = token match {
      case IDENTIFIER => s"id($name)"
      case CHARLIT => s"char($strVal)"
      case INTLIT => s"int($strVal, $base)"
      case LONGLIT => s"long($strVal, $base)"
      case FLOATLIT => s"float($strVal)"
      case DOUBLELIT => s"double($strVal)"
      case STRINGLIT => s"string($strVal)"
      case SEMI =>
        ";"
      case COMMA =>
        ","
      case _ =>
        tokenString(token)
    }

    /* Initialization: read first char, then first token */
    nextChar()
    nextToken()
  }

  private val (lastKeywordStart, kwArray) = buildKeywordArray(keywords)
}
