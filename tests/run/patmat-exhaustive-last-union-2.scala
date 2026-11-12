//> using options -Yexplicit-nulls:true

enum E[A, B]:
  case Left(a: A)
  case Right(b: B)
  case Both(a: A, b: B)

object Test:
  def f[A, B](e: E[A, B]) = e match
    case E.Left(_) => 0
    case o @ (E.Right(_) | E.Both(_, _)) => f2(o)

  def f2[A, B](e: E[A, B]) = 1

  def main(args: Array[String]): Unit =
    f(E.Left(42))
    f(E.Right(42))
    f(E.Both(42, 0))
