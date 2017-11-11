package dotty.source

object Position {

  /** Line number */
  final class LineNumber(val value: Int) extends AnyVal {
    override def toString: String = value.toString
  }

  /** Path of a source file */
  final class SourcePath(val value: String) extends AnyVal {
    override def toString: String = value
  }

  /** LineNumber containing the line number of its call site.
   *  The line number is 1-indexed.
   */
  implicit def thisLine: LineNumber = new LineNumber(0)

  /** SourcePath containing the source file of its call site. */
  implicit def thisSource: SourcePath = new SourcePath("<no source>")

}
