import scala.annotation.internal

object Test {

  object CausedBy {
    def unapply(e: Throwable): Option[Throwable] = Option(e.getCause)
  }

  @internal.link.CallGraphBounds(reachableClasses = 50, classesWithReachableMethods = 30, reachableMethods = 50)
  def main(args: Array[String]): Unit = {
    try {
        throw new Foo(new Bar)
    } catch {
        case CausedBy(x: Bar) => System.out.println(42)
    }
  }
}
