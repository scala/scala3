import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 25, classesWithReachableMethods = 7, reachableMethods = 9)
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
