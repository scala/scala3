import scala.annotation.internal

object Test {
  val f = (x: Int) => 2 * x
  val g = (x: Int) => 3 * x
  
  def lala(x: Boolean): Int=>Int = if (x) f else g

  @internal.link.CallGraphBounds(reachableClasses = 22, classesWithReachableMethods = 7, reachableMethods = 45)
  def main(args: Array[String]): Unit = {
    System.out.println(lala(true)(21))
  }
}