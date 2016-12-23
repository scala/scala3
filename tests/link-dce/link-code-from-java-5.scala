import scala.annotation.internal
import scala.math.Ordering

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 152, classesWithReachableMethods = 21, reachableMethods = 90)
  def main(args: Array[String]): Unit = {
    val arr = new Array[Object](1)
    arr(0) = 42
    java.util.Arrays.sort(arr, Ordering.Int.asInstanceOf[Ordering[Object]]) // extracted from SeqLike.sorted
    System.out.println(arr(0))
  }
}
