trait Bla1[A]:
  extension (x: A) def foo(y: A): Int
trait Bla2[A]:
  extension (x: A) def foo(y: A): Int

def test =
  given bla1[T <: Int]: Bla1[T] = ???
  given bla2[S <: Int]: Bla2[S] = ???

  1.foo(2) // error: never extension is more specific than the other
