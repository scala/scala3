import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 126, classesWithReachableMethods = 13, reachableMethods = 63)
  def main(args: Array[String]): Unit = {
    scala.collection.immutable.Vector(1, 2, 3)
  }
}
