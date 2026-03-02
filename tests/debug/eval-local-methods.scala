object Test:
  private class A:
    override def toString: String = "A"

  def main(args: Array[String]): Unit =
    val x1 = 1
    def m1(x: String) = s"m$x1($x)"
    def m2(a: A): String = s"m2($a)"
    def m3: A = new A
    println(m1("x") + m2(m3))
    val b = new B
    b.m()

class B:
  val x1 = 1
  private class C:
    override def toString: String = "C"

  def m(): Unit =
    def m1(x: String) = s"m$x1($x)"
    def m2(c: C): String = s"m2($c)"
    def m3: C = new C
    println(m1("x") + m2(m3))
