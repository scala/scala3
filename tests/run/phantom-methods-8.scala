object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    pacFun4(inky)
  }

  def pacFun4(unused clyde: Inky) = {
    println("pacFun4")
  }

  unused def inky: Inky = {
    println("inky")
    Boo.boo[Inky]
  }
}

object Boo extends Phantom {
  type Inky <: Boo.Any
  unused def boo[B <: Boo.Any]: B = assume
}
