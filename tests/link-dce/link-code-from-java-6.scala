import scala.annotation.internal

class MyInt(val x: Int) extends java.lang.Comparable[MyInt] {
  override def compareTo(that: MyInt) = this.x - that.x
  override def toString = x.toString
}

object Test {
  @internal.link.CallGraphBounds(reachableClasses = 27, classesWithReachableMethods = 10, reachableMethods = 49)
  def main(args: Array[String]): Unit = {
    val a = new Array[java.lang.Object](3)
    a(0) = new MyInt(1)
    a(1) = new MyInt(4)
    a(2) = new MyInt(3)
    java.util.Arrays.sort(a)
    System.out.println(a(0))
    System.out.println(a(1))
    System.out.println(a(2))
  }
}
