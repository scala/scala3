import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 53, classesWithReachableMethods = 42, reachableMethods = 51)
  def main(args: Array[String]): Unit = {
    scala.collection.immutable.Set.empty[Int]
  }
}
