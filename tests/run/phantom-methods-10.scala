object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun2.pacFun4(inky)
  }

  def pacFun4(unused clyde: Inky) = {
    println("pacFun4")
  }

  unused def inky: Inky = {
    println("inky")
    boo[Inky]
  }

  def fun2 = {
    println("fun")
    this
  }
}

object Boo extends Phantom {
  type Inky <: this.Any
  unused def boo[B <: this.Any]: B = assume
}
