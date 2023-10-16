trait K1Inv[F[A]]
trait K1Cov[F[+A]]
trait K1Con[F[-A]]

trait K2InvInv[F[A, B]]
trait K2InvCov[F[A, +B]]
trait K2InvCon[F[A, -B]]
trait K2CovInv[F[+A, B]]
trait K2CovCov[F[+A, +B]]
trait K2CovCon[F[+A, -B]]
trait K2ConInv[F[-A, B]]
trait K2ConCov[F[-A, +B]]
trait K2ConCon[F[-A, -B]]


trait KFunky[G[A, +B, -C, D[X1, +Y1, -Z1], +E[X2, +Y2, -Z2], -F[X3, +Y3, -Z3]]]

@main def Test =
  println(variances[K1Inv])
  println(variances[K1Cov])
  println(variances[K1Con])
  println(variances[K2InvInv])
  println(variances[K2InvCov])
  println(variances[K2InvCon])
  println(variances[K2CovInv])
  println(variances[K2CovCov])
  println(variances[K2CovCon])
  println(variances[K2ConInv])
  println(variances[K2ConCov])
  println(variances[K2ConCon])
  println(variances[KFunky])
  println(variances[[A, F[B]] =>> F[A]])
  println(variances[[A, F[+B]] =>> F[A]])
  println(variances[[A, F[-B]] =>> F[A]])
