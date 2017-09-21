object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun(phantomFun2(Boo.boo[Blinky]))
    fun(phantomFun2(Boo.boo[Inky]))
    fun(phantomFun2(Boo.boo[Pinky]))
  }

  def fun(top: Blinky): Unit = println("fun")

  def phantomFun2(p6: Blinky): Blinky = p6

}

object Boo extends Phantom {
  type Blinky <: Boo.Any
  type Inky <: Blinky
  type Pinky <: Inky
  def boo[B <: Boo.Any]: B = assume
}
