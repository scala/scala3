trait Bar[T]
object Bar {
  def derived[T]: Bar[T] = new Bar[T] {}
}

trait Baz[T]
object Baz {
  def derived[T]: Baz[T] = new Baz[T] {}
}

class Foo derives Bar, Baz

class Foo2 derives Bar,
   Baz

val x, y, z = (1, 2, 3)
val a,
    b,
    c = (1, 2, 3)