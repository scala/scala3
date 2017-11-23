object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    pacFun1(boo[Blinky])
    pacFun1(boo[Inky])
    pacFun1(boo[Pinky])
  }

  def pacFun1(unused blinky: Blinky) = {
    println("customFun1")
  }

}

object Boo extends Phantom {
  type Blinky <: Boo.Any
  type Inky <: Blinky
  type Pinky <: Inky
  unused def boo[B <: Boo.Any]: B = assume
}
