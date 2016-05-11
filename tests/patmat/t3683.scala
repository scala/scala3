sealed trait Foo
sealed trait Bar extends Foo
sealed trait W[T >: Bar <: Foo]
sealed case class X() extends W[Foo]
sealed case class Y() extends W[Bar]
sealed case class Z[T >: Bar <: Foo](
  z1: W[T]
) extends W[T]

object Main {
  def func(w: W[Bar]): Int = {
    w match {
      // Error if I include it, warning if I do not!
      // case X() => 2
      case Y() => 1
      case Z(z) => func(z)
    }
  }
}
