class A(val value: String) extends AnyVal:
  def m(size: Int): String =
    def m(mul: Int): String =
      value.take(size) * mul
    m(2)

object Test:
  def main(args: Array[String]): Unit =
    val a = new A("foo")
    println(a.m(2))
