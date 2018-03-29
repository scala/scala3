package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.embedded._

/**
 * Helper object to create virtual source files and extract markers from the source files, using the
 * `code` interpolator.
 *
 * The markers can then be used to perform tests at different positions within the source
 * file.
 */
object Code {

  // Default positions
  val m1 = new CodeMarker("m1")
  val m2 = new CodeMarker("m2")
  val m3 = new CodeMarker("m3")
  val m4 = new CodeMarker("m4")
  val m5 = new CodeMarker("m5")
  val m6 = new CodeMarker("m6")
  val m7 = new CodeMarker("m7")
  val m8 = new CodeMarker("m8")

  implicit class CodeHelper(val sc: StringContext) extends AnyVal {

    /**
     * An interpolator that lets set marker inside a virtual source file.
     *
     * For instance:
     * ```
     * code"""object ${m1}Foo${m2} { def bar = ${m3}Hello{$m4}.quux }"""
     * ```
     *
     * This will define a source file where the markers `m1` and `m2` enclose the identifier `Foo`,
     * and `m3` and `m4` enclose the identifier `Hello`. These positions can then be used to ask to
     * perform actions such as finding all references, etc.
     */
    def code(args: Embedded*): SourceWithPositions = {
      val pi = sc.parts.iterator
      val ai = args.iterator

      var line = 0
      var char = 0
      def scan(str: String): Unit = {
        for (c <- str)
          if (c == '\n') { line += 1; char = 0 } else { char += 1 }
      }

      val stringBuilder = new StringBuilder
      val positions = List.newBuilder[(CodeMarker, Int, Int)]

      while (ai.hasNext) {
        val next = pi.next().stripMargin
        stringBuilder.append(next)
        scan(next)

        ai.next() match {
          case emb: CodeMarker =>
            positions += ((emb, line, char))

          case emb: CodeInRange =>
            positions += ((emb.range.start, line, char))
            scan(emb.text)
            stringBuilder.append(emb.text)
            positions += ((emb.range.end, line, char))
        }

      }

      if (pi.hasNext)
        stringBuilder.append(pi.next())

      SourceWithPositions(stringBuilder.result(), positions.result())
    }
  }

  /** A new `CodeTester` working with `sources` in the workspace. */
  def withSources(sources: SourceWithPositions*): CodeTester = new CodeTester(sources.toList, Nil)

  /**
   * A virtual source file where several markers have been set.
   *
   * @param text      The code contained within the virtual source file.
   * @param positions The positions of the markers that have been set.
   */
  case class SourceWithPositions(text: String, positions: List[(CodeMarker, Int, Int)]) {
    /** A new `CodeTester` with only this source in the workspace. */
    def withSource: CodeTester = new CodeTester(this :: Nil, Nil)
  }

}
