package dotty.tools.dotc.core

object Positions {

  /** The bit position of the end part of a range position */
  private val Shift = 32

  class Position(val coords: Long) extends AnyVal {
    def isRange = coords < 0
    def point: Int = if (isRange) start else coords.toInt
    def start: Int = coords.abs.toInt
    def end: Int = (if (isRange) coords.abs >>> Shift else coords).toInt
  }

  class Offset(val value: Int) extends AnyVal {
    def toPosition = new Position(value.toLong & 0xffff)
  }

  def rangePos(start: Int, end: Int) =
    new Position(-(start + (end.toLong << Shift)))

  def offsetPos(point: Int) =
    new Position(point.toLong)

  val NoPosition = new Position(-1L)
  val NoOffset = new Offset(-1)
}