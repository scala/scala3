class Bar {
  implicit def f(implicit x: String): String = x

  implicitly[String](f) // error: divergent (turn -explaintypes on to see it)
}

class Foo(implicit val bar: String) {
  def this() = this("baz") // error: none of the alternatives match arguments
}

