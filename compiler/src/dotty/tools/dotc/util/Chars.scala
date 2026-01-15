package dotty.tools.dotc.util

import scala.annotation.switch
import Character.{LETTER_NUMBER, LOWERCASE_LETTER, OTHER_LETTER, TITLECASE_LETTER, UPPERCASE_LETTER}
import Character.{MATH_SYMBOL, OTHER_SYMBOL}
import Character.{isISOControl as isControl, isJavaIdentifierPart, isUnicodeIdentifierStart, isUnicodeIdentifierPart}
import java.lang.StringBuilder

/** Contains constants and classifier methods for characters */
object Chars:

  inline val LF = '\u000A'
  inline val FF = '\u000C'
  inline val CR = '\u000D'
  inline val SU = '\u001A'

  type CodePoint = Int

  /** Convert a character digit to an Int according to given base,
    *  -1 if no success
    */
  def digit2int(ch: Char, base: Int): Int = {
    val num = (
      if (ch <= '9') ch - '0'
      else if ('a' <= ch && ch <= 'z') ch - 'a' + 10
      else if ('A' <= ch && ch <= 'Z') ch - 'A' + 10
      else -1
      )
    if (0 <= num && num < base) num else -1
  }
  /** Buffer for creating '\ u XXXX' strings. */
  private val char2uescapeArray = Array[Char]('\\', 'u', 0, 0, 0, 0)

  /** Convert a character to a backslash-u escape */
  def char2uescape(c: Char): String = {
    inline def hexChar(ch: Int): Char =
      (( if (ch < 10) '0' else 'A' - 10 ) + ch).toChar

    char2uescapeArray(2) = hexChar((c >> 12)     )
    char2uescapeArray(3) = hexChar((c >>  8) % 16)
    char2uescapeArray(4) = hexChar((c >>  4) % 16)
    char2uescapeArray(5) = hexChar((c      ) % 16)

    new String(char2uescapeArray)
  }

  /** Is character a line break? */
  def isLineBreakChar(c: Char): Boolean = (c: @switch) match {
    case LF|FF|CR|SU  => true
    case _            => false
  }

  /** Is character a whitespace character (but not a new line)? */
  inline def isWhitespace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == CR

  /** Can character form part of a doc comment variable $xxx? */
  def isVarPart(c: Char): Boolean =
    '0' <= c && c <= '9' || 'A' <= c && c <= 'Z' || 'a' <= c && c <= 'z'

  /** Can character start an alphanumeric Scala identifier? */
  def isIdentifierStart(c: Char): Boolean = (c == '_') || (c == '$') || isUnicodeIdentifierStart(c)
  def isIdentifierStart(c: CodePoint): Boolean = (c == '_') || (c == '$') || isUnicodeIdentifierStart(c)

  /** Can character form part of an alphanumeric Scala identifier? */
  def isIdentifierPart(c: Char): Boolean = (c == '$') || isUnicodeIdentifierPart(c)
  def isIdentifierPart(c: CodePoint) = (c == '$') || isUnicodeIdentifierPart(c)

  /** Is character a math or other symbol in Unicode?  */
  def isSpecial(c: Char): Boolean = {
    val chtp = Character.getType(c)
    chtp == MATH_SYMBOL.toInt || chtp == OTHER_SYMBOL.toInt
  }
  def isSpecial(codePoint: CodePoint) = {
    val chtp = Character.getType(codePoint)
    chtp == MATH_SYMBOL.toInt || chtp == OTHER_SYMBOL.toInt
  }

  def isValidJVMChar(c: Char): Boolean =
    !(c == '.' || c == ';' || c =='[' || c == '/')

  def isValidJVMMethodChar(c: Char): Boolean =
    !(c == '.' || c == ';' || c =='[' || c == '/' || c == '<' || c == '>')

  def isScalaLetter(c: Char): Boolean =
    Character.getType(c: @switch) match {
      case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER | LETTER_NUMBER => true
      case _ => c == '$' || c == '_'
    }
  def isScalaLetter(c: CodePoint): Boolean =
    Character.getType(c: @switch) match {
      case LOWERCASE_LETTER | UPPERCASE_LETTER | OTHER_LETTER | TITLECASE_LETTER | LETTER_NUMBER => true
      case _ => c == '$' || c == '_'
    }

  /** Can character form part of a Scala operator name? */
  def isOperatorPart(c: Char): Boolean = (c: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '/' | '\\' => true
    case c => isSpecial(c)
  }
  def isOperatorPart(c: CodePoint): Boolean = (c: @switch) match {
    case '~' | '!' | '@' | '#' | '%' |
         '^' | '*' | '+' | '-' | '<' |
         '>' | '?' | ':' | '=' | '&' |
         '|' | '/' | '\\' => true
    case c => isSpecial(c)
  }

  /** Would the character be encoded by `NameTransformer.encode`? */
  def willBeEncoded(c: Char): Boolean = !isJavaIdentifierPart(c)

  private inline def requiresFormat(c: Char): Boolean = (c: @switch) match
    case '\b' | '\t' | '\n' | '\f' | '\r' | '"' | '\'' | '\\' => true
    case c => isControl(c)

  def escapedString(text: String, quoted: Boolean): String =
    inline def doBuild: String =
      val b = StringBuilder(text.length + 16)
      if quoted then
        b.append('"')
      var i = 0
      while i < text.length do
        escapedChar(b, text.charAt(i))
        i += 1
      if quoted then
        b.append('"')
      b.toString
    var i = 0
    while i < text.length do
      if requiresFormat(text.charAt(i)) then return doBuild
      i += 1
    if quoted then "\"" + text + "\""
    else text

  def escapedChar(ch: Char): String =
    if requiresFormat(ch) then
      val b = StringBuilder().append('\'')
      escapedChar(b, ch)
      b.append('\'').toString
    else
      "'" + ch + "'"

  private def escapedChar(b: StringBuilder, c: Char): Unit =
    inline def quadNibble(x: Int, i: Int): Unit =
      if i < 4 then
        quadNibble(x >> 4, i + 1)
        val n = x & 0xF
        val c = if (n < 10) '0' + n else 'a' + (n - 10)
        b.append(c.toChar)
    val replace = (c: @switch) match
      case '\b' => "\\b"
      case '\t' => "\\t"
      case '\n' => "\\n"
      case '\f' => "\\f"
      case '\r' => "\\r"
      case '"'  => "\\\""
      case '\'' => "\\\'"
      case '\\' => "\\\\"
      case c =>
        if isControl(c) then
          b.append("\\u")
          quadNibble(c.toInt, 0)
        else
          b.append(c)
        return
    b.append(replace)
