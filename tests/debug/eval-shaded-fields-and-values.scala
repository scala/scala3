class A:
  val x1 = "Ax1"
  val x2 = "Ax2"
  class B:
    val x2 = "Bx2"
    val x3 = "Bx3"
    def m(): Unit =
      val x3 = "x3"
      println(x1 + x2 + x3)

object Test:
  def main(args: Array[String]): Unit =
    val a = new A()
    val b = new a.B()
    b.m()
