object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun1().pacFun4(inky)
  }

  def pacFun4(clyde: Inky) = {
    println("pacFun4")
  }

  def inky: Inky = {
    println("inky")
    boo[Inky]
  }

  def fun1() = {
    println("fun")
    this
  }
}

object Boo extends Phantom {
  type Inky <: Boo.Any
  def boo[B <: Boo.Any]: B = assume
}
