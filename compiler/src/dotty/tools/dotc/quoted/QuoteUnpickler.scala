package dotty.tools.dotc.quoted

import dotty.tools.dotc.core.tasty._

object QuoteUnpickler {
  class QuotedTreeSectionUnpickler(posUnpickler: Option[PositionUnpickler], splices: Seq[Any])
    extends DottyUnpickler.TreeSectionUnpickler(posUnpickler) {
    override def unpickle(reader: TastyReader, nameAtRef: TastyUnpickler.NameTable) =
      new TreeUnpickler(reader, nameAtRef, posUnpickler, splices)
  }
}

/** A class for unpickling quoted Tasty trees and symbols.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 *  @param splices       splices that will fill the holes in the quote
 */
class QuoteUnpickler(bytes: Array[Byte], splices: Seq[Any]) extends DottyUnpickler(bytes) {
  import DottyUnpickler._
  import QuoteUnpickler._

  protected override def treeSectionUnpickler(posUnpicklerOpt: Option[PositionUnpickler]): TreeSectionUnpickler =
    new QuotedTreeSectionUnpickler(posUnpicklerOpt, splices)
}
