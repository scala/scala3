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

  def rangePos(start: Int, end: Int) =
    new Position(-(start + (end.toLong << Shift)))

  def offsetPos(point: Int) =
    new Position(point.toLong)

  val NoPosition = new Position(-1L)
}