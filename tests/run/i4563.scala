case class Foo(str: String) extends AnyVal

object Test {
  def main(args: Array[String]): Unit = {
    test(Foo("one"))
    test(Foo(""))
    test("two")
  }

  def test(x: Any): Unit = {
    val vc: Any = x match {
      case Foo("") =>
        Foo("42")
      case _ =>
        Foo("10")
    }
    println(vc)
  }
}
