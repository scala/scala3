trait F[A]
trait Z
object Z:
  given F[Z] = ???

type Foo[B] = [A] =>> Bar[A, B]
trait Bar[A, B]

given fooUnit[A: F]: Foo[Unit][A] = ???
//given bar[A: F]: Bar[A, Unit] = ???

def f[A: F](using Foo[Unit][A]): Nothing = ???

def broken: Nothing = f[Z]