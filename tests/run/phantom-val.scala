object Test {
  import Boo._

  def main(args: Array[String]): Unit = {
    println(1)
    bar(foo)
    bar(foo)
  }

  unused val foo = {
    println("foo")
    any
  }

  def bar(unused a: BooAny) = ()
}

object Boo extends Phantom {
  type BooAny = this.Any
  unused def any: BooAny = assume
}
