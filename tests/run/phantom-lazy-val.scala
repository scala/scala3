object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    println(1)
    foo
    println(2)
    foo
  }

  lazy val foo = {
    println("foo") // in erased function
    any
  }

}

object Boo extends Phantom {
  type BooAny = this.Any
  def any: BooAny = assume
}
