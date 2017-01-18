import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 23, classesWithReachableMethods = 8, reachableMethods = 42)
  def main(args: Array[String]): Unit = {
    new Baz
  }

}

class Baz extends Foo with Bar1 with Bar2

trait Bar1

trait Bar2 {
  System.out.println(42)
}