object Test {
  final class Foo(val x: String) extends AnyVal { override def toString = "" + x }
  final class Bar(val f: Foo)    extends AnyVal { override def toString = "" + f }
  def main(args: Array[String]) = {
    val x = "abc"
    val f = new Foo(x)
    val b = new Bar(f)
    assert(b.toString == "abc")
  }
}
