package dotty.tools.dotc
package util
import language.implicitConversions

/** Position format in little endian:
 *  Start: unsigned 26 Bits (works for source files up to 64M)
 *  End: unsigned 26 Bits
 *  Point: unsigned 12 Bits relative to start
 *  NoPosition encoded as -1L (this is a normally invalid position
 *  because point would lie beyond end.
 */
object Positions {

  private final val StartEndBits = 26
  private final val StartEndMask = (1L << StartEndBits) - 1
  private final val SyntheticPointDelta = (1 << (64 - StartEndBits * 2)) - 1

  /** The maximal representable offset in a position */
  private final val MaxOffset = StartEndMask

  /** Convert offset `x` to an integer by sign extending the original
   *  field of `StartEndBits` width.
   */
  def offsetToInt(x: Int) =
    x << (32 - StartEndBits) >> (32 - StartEndBits)

  /** A position indicates a range between a start offset and an end offset.
   *  Positions can be synthetic or source-derived. A source-derived position
   *  has in addition a point lies somewhere between start and end. The point
   *  is roughly where the ^ would go if an error was diagnosed at that position.
   *  All quantities are encoded opaquely in a Long.
   */
  class Position(val coords: Long) extends AnyVal {

    /** Is this position different from NoPosition? */
    def exists = this != NoPosition

    /** The start of this position. */
    def start: Int = {
      assert(exists)
      (coords & StartEndMask).toInt
    }

    /** The end of this position */
    def end: Int = {
      assert(exists)
      ((coords >>> StartEndBits) & StartEndMask).toInt
    }

    /** The point of this position, returns start for synthetic positions */
    def point: Int = {
      assert(exists)
      val poff = pointDelta
      if (poff == SyntheticPointDelta) start else start + poff
    }

    /** The difference between point and start in this position */
    def pointDelta =
      (coords >>> (StartEndBits * 2)).toInt

    def orElse(that: Position) =
      if (this.exists) this else that

    /** The union of two positions. This is the least range that encloses
     *  both positions. It is always a synthetic position.
     */
    def union(that: Position) =
      if (!this.exists) that
      else if (!that.exists) this
      else Position(this.start min that.start, this.end max that.end, this.point)

    /** Does the range of this position contain the one of that position? */
    def contains(that: Position): Boolean =
      !that.exists || exists && (start <= that.start && end >= that.end)

    /** Is this position synthetic? */
    def isSynthetic = pointDelta == SyntheticPointDelta

    /** Is this position source-derived? */
    def isSourceDerived = !isSynthetic

    /** Is this a zero-extent position? */
    def isZeroExtent = start == end

     /** A position where all components are shifted by a given `offset`
     *  relative to this position.
     */
    def shift(offset: Int) =
      if (exists) fromOffsets(start + offset, end + offset, pointDelta)
      else this

    /** The zero-extent position with start and end at the point of this position */
    def focus = if (exists) Position(point) else NoPosition

    /** The zero-extent position with start and end at the start of this position */
    def startPos = if (exists) Position(start) else NoPosition

    /** The zero-extent position with start and end at the end of this position */
    def endPos = if (exists) Position(end) else NoPosition

    /** A copy of this position with a different start */
    def withStart(start: Int) =
      if (exists) fromOffsets(start, this.end, if (isSynthetic) SyntheticPointDelta else this.point - start)
      else this

    /** A copy of this position with a different end */
    def withEnd(end: Int) =
      if (exists) fromOffsets(this.start, end, pointDelta)
      else this

    /** A copy of this position with a different point */
    def withPoint(point: Int) =
      if (exists) fromOffsets(this.start, this.end, point - this.start)
      else this

    /** A synthetic copy of this position */
    def toSynthetic = if (isSynthetic) this else Position(start, end)

    override def toString = {
      val (left, right) = if (isSynthetic) ("<", ">") else ("[", "]")
      if (exists)
        s"$left$start..${if (point == start) "" else s"$point.."}$end$right"
      else
        s"${left}no position${right}"
    }

    def toCoord: Coord =
      if (exists) new Coord(-(point + 1))
      else NoCoord
  }

  private def fromOffsets(start: Int, end: Int, pointDelta: Int) = {
    //assert(start <= end || start == 1 && end == 0, s"$start..$end")
    new Position(
      (start & StartEndMask).toLong |
      ((end & StartEndMask).toLong << StartEndBits) |
      (pointDelta.toLong << (StartEndBits * 2)))
  }

  /** A synthetic position with given start and end */
  def Position(start: Int, end: Int): Position =
    fromOffsets(start, end, SyntheticPointDelta)

  /** A source-derived position with given start, end, and point delta */
  def Position(start: Int, end: Int, point: Int): Position = {
    val pointDelta = (point - start) max 0
    fromOffsets(start, end, if (pointDelta >= SyntheticPointDelta) 0 else pointDelta)
  }

  /** A synthetic zero-extent position that starts and ends at given `start`. */
  def Position(start: Int): Position = Position(start, start)

  /** A sentinel for a non-existing position */
  val NoPosition = Position(1, 0)

  /** The coordinate of a symbol. This is either an index or
   *  a zero-range position.
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
      if (this == NoCoord) NoPosition else Position(-1 - encoding)
    }
  }

  /** An index coordinate */
  def indexCoord(n: Int): Coord = new Coord(n + 1)

  def pos2coord(pos: Position): Coord =
    if (pos.exists) new Coord(-(pos.point + 1))
    else NoCoord

  /** A sentinel for a missing coordinate */
  val NoCoord = new Coord(0)
}
