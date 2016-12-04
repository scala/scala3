import scala.annotation.internal

object Test {
  object Twice {
    def unapply(x: Int): Option[Int] = if (x % 2 == 0) Some(x / 2) else None
  }

  @internal.link.CallGraphBounds(reachableClasses = 34, classesWithReachableMethods = 13, reachableMethods = 20)
  def main(args: Array[String]): Unit = {
    val Twice(x) = 84
    System.out.println(x)
  }
}
