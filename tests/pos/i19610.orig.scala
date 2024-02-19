// Error: Cannot prove that A[U] <:< A[N].
def res1: Unit = Wrap(new A[N]{})

// Adding the obvious type argument to Wrap makes it compile just fine.
def res2: Unit = Wrap[N](new A[N]{})

final case class Wrap[T <: U](first: A[T])(using A[T] <:< A[N])

sealed trait A[+T]

type U = Any
type N = Nothing
