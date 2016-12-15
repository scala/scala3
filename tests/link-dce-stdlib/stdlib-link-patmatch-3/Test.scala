import scala.annotation.internal

object Test {
  case class Foo(x: Int)

  @internal.link.CallGraphBounds(reachableClasses = 30, classesWithReachableMethods = 25, reachableMethods = 25)
  def main(args: Array[String]): Unit = {
    Foo(42) match {
      case Foo(x) => System.out.println(x)
      case _ =>
    }
  }
}
