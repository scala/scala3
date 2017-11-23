object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun(phantomFun2(Boo.boo[Blinky]))
    fun(phantomFun2(Boo.boo[Inky]))
    fun(phantomFun2(Boo.boo[Pinky]))
  }

  def fun(unused top: Blinky): Unit = println("fun")

  unused def phantomFun2(unused p6: Blinky): Blinky = p6

}

object Boo extends Phantom {
  type Blinky <: Boo.Any
  type Inky <: Blinky
  type Pinky <: Inky
  unused def boo[B <: Boo.Any]: B = assume
}
