package scala.annotation
package internal

import language.experimental.captureChecking

/** An annotation that can be used to mark a definition as preview.
 *
 *  @see [[https://nightly.scala-lang.org/docs/reference/other-new-features/preview-defs]]
 *  @syntax markdown
 *
 *  @param message an explanation of the preview status or migration guidance
 */
private[scala] final class preview(message: String) extends StaticAnnotation:
  def this() = this("")
