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

package dotty.tools
package dotc
package core
package tasty

import util.Positions._
import collection.{mutable, Map}
import TastyBuffer.Addr

/** Unpickler for tree positions */
class PositionUnpickler(reader: TastyReader) {
  import reader._

  private[tasty] lazy val positions: Map[Addr, Position] = {
    val positions = new mutable.HashMap[Addr, Position]
    var curIndex = 0
    var curStart = 0
    var curEnd = 0
    while (!isAtEnd) {
      val header = readInt()
      val addrDelta = header >> 3
      val hasStart = (header & 4) != 0
      val hasEnd = (header & 2) != 0
      val hasPoint = (header & 1) != 0
      curIndex += addrDelta
      assert(curIndex >= 0)
      if (hasStart) curStart += readInt()
      if (hasEnd) curEnd += readInt()
      positions(Addr(curIndex)) =
        if (hasPoint) Position(curStart, curEnd, curStart + readInt())
        else Position(curStart, curEnd)
    }
    positions
  }

  def posAt(addr: Addr): Position = positions.getOrElse(addr, NoPosition)
}

