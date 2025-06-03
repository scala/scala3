//> using options -experimental

type F1Inv[A]
type F1Cov[+A]
type F1Con[-A]

type F2InvInv[A, B]
type F2InvCov[A, +B]
type F2InvCon[A, -B]
type F2CovInv[+A, B]
type F2CovCov[+A, +B]
type F2CovCon[+A, -B]
type F2ConInv[-A, B]
type F2ConCov[-A, +B]
type F2ConCon[-A, -B]

@main def Test =
  println(typeVariances[F1Inv])
  println(typeVariances[F1Cov])
  println(typeVariances[F1Con])
  println(typeVariances[F2InvInv])
  println(typeVariances[F2InvCov])
  println(typeVariances[F2InvCon])
  println(typeVariances[F2CovInv])
  println(typeVariances[F2CovCov])
  println(typeVariances[F2CovCon])
  println(typeVariances[F2ConInv])
  println(typeVariances[F2ConCov])
  println(typeVariances[F2ConCon])
