type FunctionK[A[_], B[_]] = [Z] => A[Z] => B[Z]
type ~>:[A[_], B[_]] = FunctionK[A, B]

trait RepresentableK[F[_[_], _]]:
  type RepresentationK[_]

  def tabulateK[A[_], C](f: RepresentationK ~>: A): F[A, C]

  extension[A[_], C](fa: F[A, C])
    def indexK: RepresentationK ~>: A

    def mapK[B[_]] (f: A ~>: B): F[B, C] =
      tabulateK([Z] => (r: RepresentationK[Z]) => f(indexK(r)))
