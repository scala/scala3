package dotty.tools.dotc.typer

case class Mode(val bits: Int) extends AnyVal {
  def | (that: Mode) = Mode(bits | that.bits)
  def & (that: Mode) = Mode(bits & that.bits)
  def &~ (that: Mode) = Mode(bits & ~that.bits)
  def is (that: Mode) = (bits & that.bits) == that.bits
}

object Mode {
  val None = Mode(1 << 0)

  val Expr = Mode(1 << 1)
  val Pattern = Mode(1 << 2)
  val Type = Mode(1 << 3)

  val Fun = Mode(1 << 4)


}