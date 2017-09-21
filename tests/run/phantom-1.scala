object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    fun1(Boo.any)
  }

  def fun1(boo: BooAny): Unit = {
    println("fun1")
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  def any: BooAny = assume
}
