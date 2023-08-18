package foo

enum MyEnum derives _root_.foo.Eq:
  case One

trait Eq[T]
object Eq:
  inline def derived[T](using m: scala.deriving.Mirror.Of[T]): Eq[T] = ???
