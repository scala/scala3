package scala.annotation

import language.experimental.captureChecking

/** An annotation that can be used to mark a definition as experimental.
 *
 *  @see [[https://nightly.scala-lang.org/docs/reference/other-new-features/experimental-defs]]
 *  @syntax markdown
 *
 *  @param message a description explaining the experimental status, or an empty string if no message is needed
 */
final class experimental(message: String) extends StaticAnnotation:
  def this() = this("")
