object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun(phantomFun4(boo[Blinky]))
    fun(phantomFun4(boo[Inky]))
    fun(phantomFun4(boo[Blinky]))
  }

  def fun(unused top: Blinky): Unit = println("fun")

  unused def phantomFun4[G <: Blinky](unused p8: G): G = p8

}

object Boo extends Phantom {
  type Blinky <: Boo.Any
  type Inky <: Blinky
  type Pinky <: Inky
  unused def boo[B <: Boo.Any]: B = assume
}
