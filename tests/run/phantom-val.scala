object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    println(1)
    foo
    foo

  }

  val foo = {
    println("foo")
    any
  }
}

object Boo extends Phantom {
  type BooAny = this.Any
  def any: BooAny = assume
}
