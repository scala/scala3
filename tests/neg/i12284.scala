trait I[F[_], A]  // error

def magic[F[_], A](in: I[F, A]): F[A] =  // error // error
  val deps: Vector[I[F, _]] = ??? // error
  val xx: Vector[F[_]] = deps.map(i => magic(i)) // error // error // error
  ???