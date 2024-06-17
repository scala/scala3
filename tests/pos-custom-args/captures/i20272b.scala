import language.experimental.captureChecking

trait Iterable[T] { self: Iterable[T]^ =>
  def map[U](f: T => U): Iterable[U]^{this, f}
}

object Test {
  def foo[T](level: Int, lines: Iterable[T]) =
    lines.map(x => x)

  class Bar:
    def bar(messages: Iterable[String]) =
      foo(1, messages)
  class Baz extends Bar:
    override def bar(messages: Iterable[String]) = ???
}
