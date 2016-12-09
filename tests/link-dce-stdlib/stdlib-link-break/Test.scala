import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 100, classesWithReachableMethods = 25, reachableMethods = 50)
  def main(args: Array[String]): Unit = {
    import scala.util.control.Breaks._
    breakable {
      while (true) {
        System.out.println(42)
        break()
      }
    }
  }
}
