import scala.annotation.internal

object Test {
  class Bar extends Exception("BarException")
  class Foo(e: Throwable) extends Exception(e)
  
  object CausedBy {
    def unapply(e: Throwable): Option[Throwable] = Option(e.getCause)
  }

  @internal.link.CallGraphBounds(reachableClasses = 33, classesWithReachableMethods = 14, reachableMethods = 20)
  def main(args: Array[String]): Unit = {
    try {
        throw new Foo(new Bar)
    } catch {
        case CausedBy(x: Bar) => System.out.println(42)
    }
  }
}