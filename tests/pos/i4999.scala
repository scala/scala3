trait Foo
final class Bar extends Foo

class Test {
  def test(xs: => Foo) = xs match {
    case xs: Bar => 1
    case _       => 2
  }
}
