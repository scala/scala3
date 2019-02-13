package dotty.tools
package dotc
package parsing

import scala.tasty.util.Chars._

abstract class CharArrayReader { self =>

  val buf: Array[Char]
  protected def startFrom: Int = 0

  /** Switch whether unicode should be decoded */
  protected def decodeUni: Boolean = true

  /** An error routine to call on bad unicode escapes \\uxxxx. */
  protected def error(msg: String, offset: Int): Unit

  /** the last read character */
  var ch: Char = _

  /** The offset one past the last read character */
  var charOffset: Int = startFrom

  /** The offset before the last read character */
  var lastCharOffset: Int = startFrom

  /** The start offset of the current line */
  var lineStartOffset: Int = startFrom

  /** The start offset of the line before the current one */
  var lastLineStartOffset: Int = startFrom

  private[this] var lastUnicodeOffset = -1

  /** Is last character a unicode escape \\uxxxx? */
  def isUnicodeEscape: Boolean = charOffset == lastUnicodeOffset

  /** Advance one character; reducing CR;LF pairs to just LF */
  final def nextChar(): Unit = {
    val idx = charOffset
    lastCharOffset = idx
    charOffset = idx + 1
    if (idx >= buf.length) {
      ch = SU
    } else {
      val c = buf(idx)
      ch = c
      if (c == '\\') potentialUnicode()
      else if (c < ' ') { skipCR(); potentialLineEnd() }
    }
  }

  def getc(): Char = { nextChar() ; ch }

  /** Advance one character, leaving CR;LF pairs intact.
   *  This is for use in multi-line strings, so there are no
   *  "potential line ends" here.
   */
  final def nextRawChar(): Unit = {
    val idx = charOffset
    lastCharOffset = idx
    charOffset = idx + 1
    if (idx >= buf.length) {
      ch = SU
    } else {
      val c = buf(idx)
      ch = c
      if (c == '\\') potentialUnicode()
    }
  }

  /** Interpret \\uxxxx escapes */
  private def potentialUnicode(): Unit = {
    def evenSlashPrefix: Boolean = {
      var p = charOffset - 2
      while (p >= 0 && buf(p) == '\\') p -= 1
      (charOffset - p) % 2 == 0
    }
    def udigit: Int = {
      if (charOffset >= buf.length) {
        // Since the positioning code is very insistent about throwing exceptions,
        // we have to decrement the position so our error message can be seen, since
        // we are one past EOF.  This happens with e.g. val x = \ u 1 <EOF>
        error("incomplete unicode escape", charOffset - 1)
        SU
      }
      else {
        val d = digit2int(buf(charOffset), 16)
        if (d >= 0) charOffset += 1
        else error("error in unicode escape", charOffset)
        d
      }
    }
    if (charOffset < buf.length && buf(charOffset) == 'u' && decodeUni && evenSlashPrefix) {
      do charOffset += 1
      while (charOffset < buf.length && buf(charOffset) == 'u')
      val code = udigit << 12 | udigit << 8 | udigit << 4 | udigit
      lastUnicodeOffset = charOffset
      ch = code.toChar
    }
  }

  /** replace CR;LF by LF */
  private def skipCR(): Unit = {
    if (ch == CR)
      if (charOffset < buf.length && buf(charOffset) == LF) {
        charOffset += 1
        ch = LF
      }
  }

  /** Handle line ends */
  private def potentialLineEnd(): Unit = {
    if (ch == LF || ch == FF) {
      lastLineStartOffset = lineStartOffset
      lineStartOffset = charOffset
    }
  }

  def isAtEnd: Boolean = charOffset >= buf.length

  /** A new reader that takes off at the current character position */
  def lookaheadReader(): CharArrayLookaheadReader = new CharArrayLookaheadReader

  def lookaheadChar(): Char = lookaheadReader().getc()

  class CharArrayLookaheadReader extends CharArrayReader {
    val buf: Array[Char] = self.buf
    charOffset = self.charOffset
    ch = self.ch
    override def decodeUni: Boolean = self.decodeUni
    def error(msg: String, offset: Int): Unit = self.error(msg, offset)
  }
}
