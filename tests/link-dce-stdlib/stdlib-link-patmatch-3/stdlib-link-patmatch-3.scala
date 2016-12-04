import scala.annotation.internal

object Test {
  case class Foo(x: Int)

  @internal.link.CallGraphBounds(reachableClasses = 26, classesWithReachableMethods = 12, reachableMethods = 18)
  def main(args: Array[String]): Unit = {
    Foo(42) match {
      case Foo(x) => System.out.println(x)
      case _ =>
    }
  }
}
