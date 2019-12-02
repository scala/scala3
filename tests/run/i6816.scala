trait Bar
trait Foo {
  def ==(that: Foo)(implicit b: Bar): Boolean = ???
}

case class FooCC(f: Foo)

object Test {
  def main(args: Array[String]): Unit = {
    val foo1, foo2 = new Foo {}
    assert(FooCC(foo1) == FooCC(foo1))
    assert(FooCC(foo1) != FooCC(foo2))
  }
}