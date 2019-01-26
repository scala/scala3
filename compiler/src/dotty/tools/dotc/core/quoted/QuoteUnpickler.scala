/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools.dotc.core.quoted

import dotty.tools.dotc.core.tasty._
import dotty.tools.dotc.core.tasty.TastyUnpickler.NameTable
import dotty.tools.dotc.core.tasty.TreeUnpickler.UnpickleMode

object QuoteUnpickler {
  class QuotedTreeSectionUnpickler(posUnpickler: Option[PositionUnpickler], splices: Seq[Any])
    extends DottyUnpickler.TreeSectionUnpickler(posUnpickler, None) {
    override def unpickle(reader: TastyReader, nameAtRef: NameTable): TreeUnpickler =
      new TreeUnpickler(reader, nameAtRef, posUnpickler, None, splices)
  }
}

/** A class for unpickling quoted Tasty trees and symbols. Comments are never unpickled.
 *  @param bytes         the bytearray containing the Tasty file from which we unpickle
 *  @param splices       splices that will fill the holes in the quote
 */
class QuoteUnpickler(bytes: Array[Byte], splices: Seq[Any], mode: UnpickleMode) extends DottyUnpickler(bytes, mode) {
  import DottyUnpickler._
  import QuoteUnpickler._

  protected override def treeSectionUnpickler(posUnpicklerOpt: Option[PositionUnpickler], commentUnpicklerOpt: Option[CommentUnpickler]): TreeSectionUnpickler =
    new QuotedTreeSectionUnpickler(posUnpicklerOpt, splices)
}
