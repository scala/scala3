import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 7, reachableMethods = 43)
  def main(args: Array[String]): Unit = {
    val foo = new Foo
    System.out.println(foo.bar)
    foo.baz()
    System.out.println(foo.bar)
  }
}


class Foo {
  var bar = 1

  @internal.link.AssertReachable
  def baz(): Unit = {
    bar = 42
  }
}
