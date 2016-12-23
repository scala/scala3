import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 7, reachableMethods = 11)
  def main(args: Array[String]): Unit = {
    new Foo()
  }
}

class Foo(n: Int) {
  def this() = this(42)

  @internal.link.AssertReachable def foo() = System.out.println(n)
  foo()
}
