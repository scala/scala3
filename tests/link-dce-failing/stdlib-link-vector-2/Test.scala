import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 58, classesWithReachableMethods = 7, reachableMethods = 11)
  def main(args: Array[String]): Unit = {
    scala.collection.immutable.Vector.empty[Int] :+ 1
  }
}
