package dotty.source.position

/** Path of a source file */
final class SourcePath(val value: String) extends AnyVal {
  override def toString: String = value
}

object SourcePath {
  /** SourcePath containing the source file of its call site. */
  implicit def thisSource: SourcePath = new SourcePath("<no source>")
}