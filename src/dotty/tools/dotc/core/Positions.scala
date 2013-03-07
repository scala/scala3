package dotty.tools.dotc.core

/** Position format in little endian:
 *  Start: unsigned 26 Bits (works for source files up to 64M)
 *  End: unsigned 26 Bits
 *  Point: unsigned 12 Bits relative to start
 *  NoPosition encoded as -1L (this is a normally invalid position
 *  because point would lie beyond end.
 */
object Positions {

  private val StartEndBits = 26
  private val StartEndMask = (1 << StartEndBits) - 1
  private val PointOffsetLimit = 1L << (64 - StartEndBits * 2)

  class Position(val coords: Long) extends AnyVal {
    def point: Int = start + (coords >>> (StartEndBits * 2)).toInt
    def start: Int = (coords & StartEndMask).toInt
    def end: Int = ((coords >>> StartEndBits) & StartEndMask).toInt

    /** The union of two positions. Tries to keep the point offset of
     *  the first positon if within range. Otherwise, pointoffset will be 0.
     */
    def union(that: Position) =
      if (!this.exists) that
      else if (!that.exists) this
      else {
        val start = this.start min that.start
        val end = this.end max that.end
        var pointOffset = this.point - start
        if (pointOffset >= PointOffsetLimit) pointOffset = 0
        Position(start, end, pointOffset)
      }

    def exists = this != NoPosition
  }

  def Position(start: Int, end: Int, pointOffset: Int = 0): Position =
    new Position(
      (start & StartEndMask).toLong |
      ((end & StartEndMask).toLong << StartEndBits) |
      (pointOffset.toLong << (StartEndBits * 2)))

  def Position(point: Int): Position = Position(point, point, 0)

  val NoPosition = new Position(-1L)

  /** The coordinate of a symbol. This is either an index or
   *  a point position.
   */
  class Coord(val encoding: Int) extends AnyVal {
    def isIndex = encoding > 0
    def isPosition = encoding <= 0
    def toIndex: Int = {
      assert(isIndex)
      encoding - 1
    }
    def toPosition = {
      assert(isPosition)
      if (this == NoCoord) NoPosition else Position(1 - encoding)
    }
  }

  def indexCoord(n: Int) = new Coord(n + 1)
  def positionCoord(n: Int) = new Coord(-(n + 1))

  val NoCoord = new Coord(0)
}