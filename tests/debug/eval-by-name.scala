object Test:
  def main(args: Array[String]): Unit =
    println(m("foo") + A("bar").m)

  def m(x: => String): String =
    def m: String =
      x + x
    class A:
      def m: String =
        x.take(2)
    m + A().m

class A(x: => String):
  def m: String = x
