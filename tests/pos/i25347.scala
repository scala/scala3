package p

private[p] object sk3:
  class Foo
  extension (x: Foo) def bar: String = "hi"

object Test:
  import sk3.Foo
  def test(x: Foo): String = x.bar
