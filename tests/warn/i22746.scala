
//> using options -Wunused:all -Werror

import java.time.ZonedDateTime

trait Foo[A] {
  def apply(a: A, t: ZonedDateTime): A
}

extension [A](a: A)(using f: Foo[A]) {
  def foo(t: ZonedDateTime = ZonedDateTime.now): A = f(a, t)
}

def test[I, A](in: I)(
  run: I => Either[Throwable, A],
  onErr: Throwable => Throwable = identity[Throwable]
): Either[Throwable, A] =
  run(in) match {
    case Left(t) => Left(onErr(t))
    case r @ Right(_) => r
  }
