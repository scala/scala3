package scala.annotation

/** An annotation that can be used to mark a definition as preview.
 *
 *  @see [[https://dotty.epfl.ch/docs/reference/other-new-features/preview-defs]]
 *  @syntax markdown
 */
final class preview(message: String) extends StaticAnnotation:
  def this() = this("")
