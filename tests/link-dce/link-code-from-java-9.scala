import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 7, reachableMethods = 47)
  def main(args: Array[String]): Unit = {
    foo(new Foo)
  }

  def foo(s: Object) = {
    System.out.println(s)
  }

}

class Foo {
  @internal.link.AssertReachable
  override def toString: String = "Foo.toString"
}
