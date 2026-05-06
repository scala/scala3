package scala.annotation

import language.experimental.captureChecking

/** An annotation that can be used to mark a definition as experimental.
 *
 *  @see [[https://nightly.scala-lang.org/docs/reference/other-new-features/experimental-defs]]
 *  @syntax markdown
 */
final class experimental(message: String) extends StaticAnnotation:
  def this() = this("")
