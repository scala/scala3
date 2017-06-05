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

  /**
   * Shorthand to construct a filter in the common case where you're
   * switching on the prefix of the input stream and want to run some
   * transformation on the buffer/cursor
   */
  def Case(s: String)
          (f: (Vector[Char], Int, TermInfo) => (Vector[Char], Int)) = new Filter {
    val op = new PartialFunction[TermInfo, TermAction] {
      def isDefinedAt(x: TermInfo) = {

        def rec(i: Int, c: LazyList[Int]): Boolean = {
          if (i >= s.length) true
          else if (c.head == s(i)) rec(i + 1, c.tail)
          else false
        }
        rec(0, x.ts.inputs)
      }

      def apply(v1: TermInfo) = {
        val (buffer1, cursor1) = f(v1.ts.buffer, v1.ts.cursor, v1)
        TermState(
          v1.ts.inputs.dropPrefix(s.map(_.toInt)).get,
          buffer1,
          cursor1
        )
      }

    }.lift
    def identifier = "Case"
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
