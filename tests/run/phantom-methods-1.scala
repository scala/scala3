object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun(phantomFun1())
  }

  def fun(unused top: BooAny): Unit = ()

  unused def phantomFun1(): Pinky = boo[Pinky]
}

object Boo extends Phantom {
  type BooAny = Boo.Any
  type Pinky <: Boo.Any
  unused def boo[B <: Boo.Any]: B = assume
}
