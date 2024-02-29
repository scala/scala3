// A minimised reproduction of how an initial change to combineEligibles broke Typer#findRef
case class Foo(n: Int)

class Test:
  import this.toString

  val foo1 = Foo(1)
  val foo2 = Foo(2)

  def foo(using Foo): Foo =
    import this.*
    def bar(using Foo): Foo = summon[Foo]
    bar(using foo2)

object Test extends Test:
  def main(args: Array[String]): Unit =
    assert(foo(using foo1) eq foo2)
