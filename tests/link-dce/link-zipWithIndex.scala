import scala.annotation.internal

object Test {
 // @internal.link.CallGraphBounds(reachableClasses = 31, classesWithReachableMethods = 7, reachableMethods = 55)
  def main(args: Array[String]): Unit = {
    val list = List('a', 'b', 'c')
    for ((elem, idx) <- list.zipWithIndex) {
      System.out.println(elem + " " + idx)
    }
//    val s = list.zip(list)
//    s.withFilter(_ => true)
  }

}
