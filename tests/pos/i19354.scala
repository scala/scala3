class Foo; class Bar
class Test:
  def t1(xs: java.util.stream.Stream[? <: Foo]) =
    xs.map(x => take(x))

  def take(x: Foo) = ""
  def take(x: Bar) = ""
