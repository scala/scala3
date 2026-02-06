class Blah extends scala.annotation.StaticAnnotation

trait Barly {
  def bar[T](a: String, @Foo v: Int)(@Foo b: T, @Blah w: Int) = ()
  extension [T](s: String) def f[U](@Foo t: T, @Foo u: U) = ()
}

class Bar extends Barly {
  def bar2(@Foo v: Int) = ()
}
