// scalac: -Werror

trait Outer[F[_]]:
  sealed trait Inner
  trait Inner1 extends Inner
  def foo(rv: Either[Inner, Int]) =
    rv match
      case Right(_) =>
      case Left(_) =>
