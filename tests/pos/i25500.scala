trait A[B[_]]:
  type C

object A:
  given A[Option]:
    type C = Option[Int]

  def apply[E[_], F](e: E[F])(using a: A[E]): a.C = ???

def apply[D[_]](other: (a: A[D]) ?=> a.C): Any = ???

def main =
  apply(A(Option(5)))