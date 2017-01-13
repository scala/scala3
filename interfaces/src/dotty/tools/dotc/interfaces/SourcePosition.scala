package dotty.tools.dotc.interfaces

import scala.annotation.binaryCompatible


/** A position in a source file.
 *
 * A position is a range between a start offset and an end offset, as well as a
 * point inside this range.
 *
 * As a convenience, we also provide methods that return the line and the column
 * corresponding to each offset.
 *
 * User code should not implement this interface, but it may have to
 * manipulate objects of this type.
 */
@binaryCompatible
trait SourcePosition {
  /** @return Content of the line which contains the point */
  def lineContent(): String

  /** @return Offset to the point */
  def point(): Int

  /** @return Line number of the point, starting at 0 */
  def line(): Int

  /** @return Column number of the point, starting at 0 */
  def column(): Int

  /** @return Offset to the range start */
  def start(): Int

  /** @return Line number of the range start, starting at 0 */
  def startLine(): Int

  /** @return Column number of the range start, starting at 0 */
  def startColumn(): Int

  /** @return Offset to the range end */
  def end(): Int

  /** @return Line number of the range end, starting at 0 */
  def endLine(): Int

  /** @return Column number of the range end, starting at 0 */
  def endColumn(): Int

  /** The source file corresponding to this position.
   * The values returned by `point()`, `start()` and `end()`
   * are indices in the array returned by `source().content()`.
   * @return source file for this position
   */
  def source(): SourceFile
}
