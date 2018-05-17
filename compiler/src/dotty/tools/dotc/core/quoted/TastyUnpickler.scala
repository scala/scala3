package dotty.tools.dotc.core.quoted

import dotty.tools.dotc.core.tasty._
import dotty.tools.dotc.core.tasty.TastyUnpickler.NameTable

object TastyUnpickler {
  class QuotedTreeSectionUnpickler(posUnpickler: Option[PositionUnpickler], splices: Seq[Any])
    extends DottyUnpickler.TreeSectionUnpickler(posUnpickler, None) {
    override def unpickle(reader: TastyReader, nameAtRef: NameTable) =
      new TreeUnpickler(reader, nameAtRef, posUnpickler, None, splices)
  }
}

/** A class for unpickling quoted Tasty trees and symbols. Comments are never unpickled.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 *  @param splices       splices that will fill the holes in the quote
 */
class TastyUnpickler(bytes: Array[Byte], splices: Seq[Any], isTypeTree: Boolean) extends DottyUnpickler(bytes) {
  import DottyUnpickler._
  import TastyUnpickler._

  override protected def mode: TreeUnpickler.UnpickleMode =
    if (isTypeTree) TreeUnpickler.UnpickleMode.TypeTree else TreeUnpickler.UnpickleMode.Term

  protected override def treeSectionUnpickler(posUnpicklerOpt: Option[PositionUnpickler], commentUnpicklerOpt: Option[CommentUnpickler]): TreeSectionUnpickler =
    new QuotedTreeSectionUnpickler(posUnpicklerOpt, splices)
}
