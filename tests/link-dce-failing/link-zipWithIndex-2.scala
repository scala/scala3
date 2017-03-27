import scala.annotation.internal

object Test {
 // @internal.link.CallGraphBounds(reachableClasses = 31, classesWithReachableMethods = 7, reachableMethods = 55)
  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c')
    list.zipWithIndex.map { case (pname, idx) =>
      System.out.println(pname + " " + idx)
    }
  }

}
