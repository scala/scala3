sealed trait Mark[T]

trait Foo[T]
class Bar1[T] extends Foo[T]
class Bar2[T] extends Foo[T] with Mark[T]

class Test:
  def t1(foo: Foo[Int]): Unit = foo match
    case _: Mark[t] =>
    case _          =>

  def t2[F <: Foo[Int]](foo: F): Unit = foo match
    case _: Mark[t] =>
    case _          =>
