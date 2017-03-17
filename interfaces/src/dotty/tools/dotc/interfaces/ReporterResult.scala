package dotty.tools.dotc.interfaces

import scala.annotation.binaryCompatible


/** Summary of the diagnostics emitted by a Reporter.
 *
 * User code should not implement this interface, but it may have to
 * manipulate objects of this type.
 */
@binaryCompatible
trait ReporterResult {
  /** @return Have we emitted any error? */
  def hasErrors(): Boolean

  /** @return Number of errors that have been emitted */
  def errorCount(): Int

  /** @return Have we emitted any warning ? */
  def hasWarnings(): Boolean

  /** @return Number of warnings that have been emitted */
  def warningCount(): Int
}
