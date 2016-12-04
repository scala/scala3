import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 52, classesWithReachableMethods = 7, reachableMethods = 6)
  def main(args: Array[String]): Unit = {
    scala.collection.mutable.Set.empty[Int]
  }
}
