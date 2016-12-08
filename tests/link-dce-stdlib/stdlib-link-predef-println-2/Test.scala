import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 105, classesWithReachableMethods = 80, reachableMethods = 147)
  def main(args: Array[String]): Unit = {
    println(new Foo)
  }
}
