package scala.annotation

/** An annotation that can be used to mark a definition as experimental.
 *
 *  @see [[https://dotty.epfl.ch/docs/reference/other-new-features/experimental-defs]]
 *  @syntax markdown
 */
final class experimental(message: String) extends StaticAnnotation:
  def this() = this("")
