final case class Wrap[T <: U](first: A[T])(using A[T] <:< A[N])
sealed trait A[+T]
type U = Any
type N = Nothing

@main def main = {
  // Error: Cannot prove that A[U] <:< A[N].
  Wrap(new A[N] {})
}
