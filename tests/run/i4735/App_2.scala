import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val arr = Array.tabulate[Int](21)(x => x)
    for (x <- new Unrolled(arr)) {
      System.out.println(2*x)
    }
  }
}

class Unrolled(arr: Array[Int]) extends AnyVal {
  inline def foreach(f: => Int => Unit): Unit = Macro.unrolledForeach(3, arr, f)
}
