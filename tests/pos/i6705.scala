trait StringTempl {
  def mkString: String
  def mkString(x: String): String
}


object Test {
  extension (x: String) def shouldBe(y: String): Boolean = ???

  def test(tmpl: StringTempl): Unit = {
    tmpl.mkString shouldBe "hello"                       // error
    tmpl.mkString(", world") shouldBe "hello, world"
  }
}