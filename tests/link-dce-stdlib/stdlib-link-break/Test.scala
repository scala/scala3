import scala.annotation.internal

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 70, classesWithReachableMethods = 17, reachableMethods = 29)
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
