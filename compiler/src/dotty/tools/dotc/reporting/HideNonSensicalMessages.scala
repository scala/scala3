package dotty.tools
package dotc
package reporting

import core.Contexts.*

/**
 * This trait implements `isHidden` so that we avoid reporting non-sensical messages.
 */
trait HideNonSensicalMessages extends Reporter {
  /** Hides non-sensical messages, unless we haven't reported any error yet or
   *  `-Yshow-suppressed-errors` is set.
   */
  override def isHidden(dia: Diagnostic)(using Context): Boolean =
    super.isHidden(dia) || {
        (if !errorsReported && dia.isInstanceOf[Diagnostic.Error] then
          // Bump up errorCount so hasErrors is true while forcing the message.
          // We use errorsReported as a predicate for broken code.
          // So now any forcing won't cause, for instance,
          // assertion errors and thus compiler crashes.
          // Some messages, once forced, run more code
          // to generate useful hints for the user.
          try
            _errorCount += 1
            dia.msg.isNonSensical
          finally _errorCount -= 1 // decrease rather than reset the value so we only ever decrease by 1
        else dia.msg.isNonSensical) &&
        hasErrors && // if there are no errors yet, report even if diagnostic is non-sensical
        !ctx.settings.YshowSuppressedErrors.value
    }
}
