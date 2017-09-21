object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    pacFun3(boo[Clyde])
    pacFun3(boo[Pinky])
  }

  def pacFun3(clyde: Clyde) = {
    println("pacFun3")
  }
}

object Boo extends Phantom {
  type Inky <: Boo.Any
  type Pinky <: Inky
  type Clyde >: Pinky <: Inky
  def boo[B <: Boo.Any]: B = assume
}
