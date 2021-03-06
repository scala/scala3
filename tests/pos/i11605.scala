package example

class Extractor[A](val get: A) extends AnyVal {
  // https://github.com/scala/scala/pull/9343
  def isEmpty: false = false
}

object Foo {
  def unapply(x: String): Extractor[String] = new Extractor(x)
}

object Main {
  val Foo(a) = "a"
}