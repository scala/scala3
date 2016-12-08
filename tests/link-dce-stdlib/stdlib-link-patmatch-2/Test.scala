import scala.annotation.internal

object Test {
  object Twice {
    def unapply(x: Int): Option[Int] = if (x % 2 == 0) Some(x / 2) else None
  }

  @internal.link.CallGraphBounds(reachableClasses = 37, classesWithReachableMethods = 19, reachableMethods = 33)
  def main(args: Array[String]): Unit = {
    val Twice(x) = 84
    System.out.println(x)
  }
}
