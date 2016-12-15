import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 120, classesWithReachableMethods = 100, reachableMethods = 150)
  def main(args: Array[String]): Unit = {
    println(new Foo)
  }
}
