import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 7, reachableMethods = 10)
  def main(args: Array[String]): Unit = {
    new Foo()
  }
}

class Foo(n: Int) {
  def this() = this(42)

  @internal.link.AssertReachable def foo() = System.out.println(n)
  foo()
}
