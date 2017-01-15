import scala.annotation.{internal, tailrec}

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 20, classesWithReachableMethods = 6, reachableMethods = 40)
  def main(args: Array[String]): Unit = {
    var i = 0
    while (i < 5) {
      System.out.println(i)
      i += 1
    }
  }
}
