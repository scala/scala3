class A:
  private val a = "a"
  class B:
    class C:
      def m: String =
        a + a

object Test:
  def main(args: Array[String]): Unit =
    val a = new A
    val b = new a.B
    val c = new b.C
    println(c.m)
