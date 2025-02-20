class A:
  val x1 = "Ax1"
  def m(): Unit =
    val x1 = "x1"
    class B:
      val x2 = "Bx2"
      def m(): Unit =
        val x2 = "x2"
        println(x1 + A.this.x1)
      override def toString: String = "B"
    val b = new B
    b.m()

object Test:
  def main(args: Array[String]): Unit =
    val a = new A
    a.m()
