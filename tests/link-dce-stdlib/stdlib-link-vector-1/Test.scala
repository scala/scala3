import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 100, classesWithReachableMethods = 60, reachableMethods = 100)
  def main(args: Array[String]): Unit = {
    scala.collection.immutable.Vector.empty
  }
}
