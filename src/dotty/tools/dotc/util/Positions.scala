package dotty.tools.dotc
package util

/** Position format in little endian:
 *  Start: unsigned 26 Bits (works for source files up to 64M)
 *  End: unsigned 26 Bits
 *  Point: unsigned 12 Bits relative to start
 *  NoPosition encoded as -1L (this is a normally invalid position
 *  because point would lie beyond end.
 */
object Positions {

  private val SyntheticPointDelta = 1 << (64 - StartEndBits * 2) - 1
  private val StartEndBits = 26
  private val StartEndMask: Long = (1L << StartEndBits) - 1

  /** A position indicates a range between a start offset and an end offset.
   *  Positions can be synthetic or source-derived. A source-derived position
   *  has in addition a pointlies somewhere between start and end. The point
   *  is roughly where the ^ would go if an error was diagnosed at that position.
   *  All quantities are encoded opaquely in a Long.
   */
  class Position(val coords: Long) extends AnyVal {

    /** The start of this position. */
    def start: Int = (coords & StartEndMask).toInt

    /** The point of this position, returns start for synthetic positions */
    def point: Int = {
      val poff = pointDelta
      if (poff == SyntheticPointDelta) start else start + poff
    }

    /** The end of this position */
    def end: Int = ((coords >>> StartEndBits) & StartEndMask).toInt

    /** The difference between point and start in this position */
    def pointDelta = (coords >>> (StartEndBits * 2)).toInt

    /** The union of two positions. This is the least range that encloses
     *  both positions. It is always a synthetic position.
     */
    def union(that: Position) =
      if (!this.exists) that
      else if (!that.exists) this
      else Position(this.start min that.start, this.end max that.end)

    /** Does the range of this position contain the one of that position? */
    def contains(that: Position): Boolean =
      if (exists) (start <= that.start && end >= that.end)
      else !that.exists

    /** Is this position different from NoPosition? */
    def exists = this != NoPosition

    /** Is this position synthetic? */
    def isSynthetic = pointDelta == SyntheticPointDelta

    /** Is this position source-derived? */
    def isSourceDerived = !isSynthetic

     /** A position where all components are shifted by a given `offset`
     *  relative to this position.
     */
    def shift(offset: Int) =
      if (exists) Position(start + offset, end + offset, pointDelta)
      else this

    /** The zero-extent position with start and end at the point of this position */
    def focus = Position(point)

    /** The zero-extent position with start and end at the start of this position */
    def startPos = Position(start)

    /** The zero-extent position with start and end at the end of this position */
    def endPos = Position(end)

    /** A copy of this position with a different start */
    def withStart(start: Int) = Position(start, this.end, this.point - start)

    /** A copy of this position with a different end */
    def withEnd(end: Int) = Position(this.start, end, this.point - this.start)

    /** A copy of this position with a different point */
    def withPoint(point: Int) =
      Position(this.start, this.end, point - this.start)

    /** A synthetic copy of this position */
    def toSynthetic = if (isSynthetic) this else Position(start, end)
  }

  private def fromOffsets(start: Int, end: Int, pointDelta: Int) =
    new Position(
      (start & StartEndMask).toLong |
      ((end & StartEndMask).toLong << StartEndBits) |
      (pointDelta.toLong << (StartEndBits * 2)))

  /** A synthetic position with given start and end */
  def Position(start: Int, end: Int): Position =
    fromOffsets(start, end, SyntheticPointDelta)

  /** A source-derived position with given start, end, and point delta */
  def Position(start: Int, end: Int, pointDelta: Int): Position =
    fromOffsets(start, end, if (pointDelta >= SyntheticPointDelta) 0 else pointDelta)

  /** A synthetic zero-extent position that starts and ends at given `start`. */
  def Position(start: Int): Position = Position(start, start, 0)

  /** A sentinal for a non-existing position */
  val NoPosition = Position(1, 0)

  /** A source position is comprised of a position in a source file */
  case class SourcePosition(source: SourceFile, pos: Position) {
    def point: Int = pos.point
    def start: Int = pos.start
    def end: Int = pos.end
    def exists = pos.exists
  }

  /** A sentinel for a non-existing source position */
  val NoSourcePosition = SourcePosition(NoSource, NoPosition)

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
      if (this == NoCoord) NoPosition else Position(1 - encoding)
    }
  }

  /** An index coordinate */
  def indexCoord(n: Int) = new Coord(n + 1)
  def positionCoord(pos: Position) = new Coord(-(pos.point + 1))

  /** A sentinel for a missing coordinate */
  val NoCoord = new Coord(0)
}