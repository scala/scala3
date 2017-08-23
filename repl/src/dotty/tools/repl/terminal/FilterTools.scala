package dotty.tools
package repl
package terminal

/**
 * A collection of helpers that to simpify the common case of building filters
 */
object FilterTools {
  val ansiRegex = "\u001B\\[[;\\d]*."

  def offsetIndex(buffer: Vector[Char], in: Int) = {
    var splitIndex = 0
    var length = 0

    while(length < in) {
      ansiRegex.r.findPrefixOf(buffer.drop(splitIndex)) match {
        case None =>
          splitIndex += 1
          length += 1
        case Some(s) =>
          splitIndex += s.length
      }
    }
    splitIndex
  }

  /** Shorthand for pattern matching on [[TermState]] */
  val TS = TermState

  def findChunks(b: Vector[Char], c: Int) = {
    val chunks = Terminal.splitBuffer(b)
    // The index of the first character in each chunk
    val chunkStarts = chunks.inits.map(x => x.length + x.sum).toStream.reverse
    // Index of the current chunk that contains the cursor
    val chunkIndex = chunkStarts.indexWhere(_ > c) match {
      case -1 => chunks.length-1
      case x => x - 1
    }
    (chunks, chunkStarts, chunkIndex)
  }

  def firstRow(cursor: Int, buffer: Vector[Char], width: Int) =
    cursor < width && (buffer.indexOf('\n') >= cursor || buffer.indexOf('\n') == -1)

  def lastRow(cursor: Int, buffer: Vector[Char], width: Int) =
    (buffer.length - cursor) < width &&
      (buffer.lastIndexOf('\n') < cursor || buffer.lastIndexOf('\n') == -1)
}
