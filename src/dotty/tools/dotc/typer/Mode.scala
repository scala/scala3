package dotty.tools.dotc.typer

import collection.mutable

case class Mode(val bits: Int) extends AnyVal {
  import Mode._
  def | (that: Mode) = Mode(bits | that.bits)
  def & (that: Mode) = Mode(bits & that.bits)
  def &~ (that: Mode) = Mode(bits & ~that.bits)
  def is (that: Mode) = (bits & that.bits) == that.bits

  def isExpr = (this & PatternOrType) == None

  override def toString =
    (0 until 31).filter(i => (bits & (1 << i)) != 0).map(modeName).mkString("Mode(", ",", ")")
}

object Mode {
  val None = Mode(0)

  private var modeName = new Array[String](32)

  def newMode(bit: Int, name: String): Mode = {
    modeName(bit) = name
    Mode(1 << bit)
  }

  val Pattern = newMode(0, "Pattern")
  val Type = newMode(1, "Type")

  val ImplicitsEnabled = newMode(2, "ImplicitsEnabled")
  val InferringReturnType = newMode(3, "InferringReturnType")

  val TypevarsMissContext = newMode(4, "TypevarsMissContext")
  val CheckCyclic = newMode(5, "CheckCyclic")

  val PatternOrType = Pattern | Type
}