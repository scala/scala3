package dotty.source.position

/** Line number */
final class LineNumber(val value: Int) extends AnyVal {
  override def toString: String = value.toString
}

object LineNumber {
  /** LineNumber containing the line number of its call site.
   *  The line number is 1-indexed.
   */
  implicit def thisLine: LineNumber = new LineNumber(0)
}
