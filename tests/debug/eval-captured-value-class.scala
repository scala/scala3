class Size(val value: Int) extends AnyVal

object Test:
  def main(args: Array[String]): Unit =
    val size = new Size(2)

    class A(msg: String):
      override def toString: String =
        msg.take(size.value)

    def m(msg: String): String =
      msg.take(size.value)

    println(new A("foo"))
    println(m("bar"))
