package scala.annotation

import language.experimental.captureChecking
import meta.exportable

/** An annotation that can be used to mark a definition as experimental.
 *
 *  @see [[https://dotty.epfl.ch/docs/reference/other-new-features/experimental-defs]]
 *  @syntax markdown
 */
@exportable
final class experimental(message: String) extends StaticAnnotation:
  def this() = this("")
