import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 100, classesWithReachableMethods = 50, reachableMethods = 60)
  def main(args: Array[String]): Unit = {
    scala.collection.immutable.Set.empty[Int]
  }
}
