class A(val value: String) extends AnyVal:
  def m(size: Int): String =
    class B:
      def m: String =
        value.take(size)
    (new B).m

object Test:
  def main(args: Array[String]): Unit =
    val a = new A("foo")
    println(a.m(2))
