class Foo(val config: String) {
  case class Bar(val x: Int) {
    def doThings: String = config //Do whatever here
  }
}


object Test {
  def test(foo: Foo)(bar: foo.Bar = foo.Bar(5))(implicit s: String = "ok") = s + foo.config + bar.x

  def main(args: Array[String]) = {
    val res = test(new Foo("port"))()
    assert(res == "okport5")
  }
}
