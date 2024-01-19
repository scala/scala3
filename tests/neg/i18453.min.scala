// Slightly nicer version of i18453
// which uses a non-abstract type Foo instead
trait Box[T]

trait Foo

class Test:
  def meth[A](func: A => A & Foo)(using boxA: Box[A]): Unit = ???
  def test[B]                    (using boxB: Box[B]): Unit =
    def nest(p: B): B & Foo = ???
    meth(nest) // error
