import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 21, classesWithReachableMethods = 7, reachableMethods = 10)
  def main(args: Array[String]): Unit = {
    System.out.println(new Foo(42).x)
  }
}

class Foo(x: Int) extends Bar(x)
class Bar(@internal.link.AssertReachable val x: Int)
