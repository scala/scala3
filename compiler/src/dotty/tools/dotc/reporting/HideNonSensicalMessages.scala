package dotty.tools
package dotc
package reporting

import core.Contexts.Context
import diagnostic.Diagnostic

/**
 * This trait implements `isHidden` so that we avoid reporting non-sensical messages.
 */
trait HideNonSensicalMessages extends Reporter {
  /** Hides non-sensical messages, unless we haven't reported any error yet or
   *  `-Yshow-suppressed-errors` is set.
   */
  override def isHidden(dia: Diagnostic)(implicit ctx: Context): Boolean =
    super.isHidden(dia) || {
        dia.isNonSensical &&
        hasErrors && // if there are no errors yet, report even if diagnostic is non-sensical
        !ctx.settings.YshowSuppressedErrors.value
    }
}
