
import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val arr = Array.tabulate[Int](21)(x => x)
    Macro.unrolledForeach(3, arr) { (x: Int) =>
      System.out.println(2 * x)
    }

    /* unrooled code:

      val size: Int = arr.length()
      assert(size % 3 == 0)
      var i: Int = 0
      while (i < size) {
        println("<log> start loop")
        val element$1: Int = arr(i)
        val x$1: Int = element$1
        System.out.println(2 * x$1)
        val element$2: Int = arr(i + 1)
        val x$2: Int = element$2
        System.out.println(2 * x$2)
        val element$3: Int = arr(i + 2)
        val x$3: Int = element$3
        System.out.println(2 * x$3)
        i = i + 3
      }
    */
  }

}
