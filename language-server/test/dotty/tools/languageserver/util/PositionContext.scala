package dotty.tools.languageserver.util

import dotty.tools.languageserver.util.embedded.CodeMarker
import dotty.tools.languageserver.util.server.TestFile

import scala.compiletime.uninitialized

class PositionContext(positionMap: Map[CodeMarker, (TestFile, Int, Int)]) {
  private var lastKey: CodeMarker = uninitialized
  private var lastValue: (TestFile, Int, Int) = uninitialized
  def positionOf(pos: CodeMarker): (TestFile, Int, Int) = {
    if (lastKey eq pos) lastValue
    else {
      lastValue = positionMap.getOrElse(pos,
        { assert(false, "CodePosition was not found in the code: " + pos); null }
      )
      lastKey = pos
      lastValue
    }
  }

  def contains(pos: CodeMarker): Boolean = positionMap.contains(pos)

  def withPos(marker: CodeMarker, pos: (TestFile, Int, Int)) =
    new PositionContext(positionMap.updated(marker, pos))
}

object PositionContext {
  type PosCtx[T] = PositionContext ?=> T
}
