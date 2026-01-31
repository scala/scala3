//> using options -Xkind-projector

def summonClock[F[_] : Clock] = ???
def testsummon = {
  summonClock // error
}

trait Monad[F[_]]
implicit def inst1[F[_]]: Monad[F] = ???
implicit def inst2[F[_]]: Monad[F] = ???

trait Clock[F[_]]
object Clock {
  trait A[F[_], T]
  trait B[F[_], T]
  trait C[F[_], T]
  trait D[F[_], T]
  trait E[F[_], T]
  trait F1[F[_], T]
  trait G[F[_], T]
  trait H[F[_], T]

  implicit def clockForOptionT[F[_]](implicit F0: Monad[F],
                                      C0: Clock[F]): Clock[A[F, *]] = ???
  implicit def clockForEitherT[F[_]](implicit F0: Monad[F],
                                      C0: Clock[F]): Clock[B[F, *]] = ???
  implicit def clockForStateT[F[_]](implicit F0: Monad[F],
                                     C0: Clock[F]): Clock[C[F, *]] = ???
  implicit def clockForWriterT[F[_]](implicit F0: Monad[F],
                                     C0: Clock[F]): Clock[D[F, *]] = ???
  implicit def clockForIorT[F[_]](implicit F0: Monad[F],
                                  C0: Clock[F]): Clock[E[F, *]] = ???

  implicit def clockForKleisli[F[_]](implicit F0: Monad[F],
                                     C0: Clock[F]): Clock[F1[F, *]] = ???
  implicit def clockForContT[F[_]](implicit F0: Monad[F],
                                   C0: Clock[F]): Clock[G[F, *]] = ???
  implicit def clockForReaderWriterStateT[F[_]](implicit F0: Monad[F],
                                                C0: Clock[F]): Clock[H[F, *]] = ???
}
