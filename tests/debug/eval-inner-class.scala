object Test:
  def main(args: Array[String]): Unit =
    val test = new Test
    test.m()

  private def a1: A = new A
  private def a2(a: A): String = "a2"

  class A:
    val a3: String = "a3"
    def a3(x: String): String = s"a3($x)"
    override def toString: String = "A"
end Test

class Test:
  def m(): Unit =
    println("test.m()")

  private def b1: B = new B
  private def b2(b: B) = "b2"

  private class B:
    val b3: String = "b3"
    def b3(x: String): String = s"b3($x)"
    override def toString: String = "B"
