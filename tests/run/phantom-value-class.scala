
object Test {
  def main(args: Array[String]): Unit = {
    new Foo(Boo.any).foo()
  }
}

class Foo(x: Boo.BooAny) extends AnyVal {
  def foo(): Unit = println("foo")
}

object Boo extends Phantom {
  type BooAny = this.Any
  def any: BooAny = assume
}
