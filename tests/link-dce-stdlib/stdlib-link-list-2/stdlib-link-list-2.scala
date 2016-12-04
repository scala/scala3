import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 54, classesWithReachableMethods = 7, reachableMethods = 8)
  def main(args: Array[String]): Unit = {
    1 :: scala.collection.immutable.List.empty[Int]
  }
}
