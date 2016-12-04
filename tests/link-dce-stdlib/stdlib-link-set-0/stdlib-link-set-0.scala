import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 49, classesWithReachableMethods = 7, reachableMethods = 7)
  def main(args: Array[String]): Unit = {
    scala.collection.immutable.Set.empty[Int]
  }
}
