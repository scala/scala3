import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 100, classesWithReachableMethods = 20, reachableMethods = 50)
  def main(args: Array[String]): Unit = {
    scala.collection.mutable.Set.empty[Int]
  }
}
