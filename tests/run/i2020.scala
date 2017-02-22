object Test {

  case class Foo(x: Int)(y: Int)

  def main(args: Array[String]) =
    assert(Foo(1)(1) == Foo(1)(2))

}
