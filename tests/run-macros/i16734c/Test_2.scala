class C1Inv[A] { type T }
class C1Cov[+A] { type T }
class C1Con[-A] { type T }

class C2InvInv[A, B] { type T }
class C2InvCov[A, +B] { type T }
class C2InvCon[A, -B] { type T }
class C2CovInv[+A, B] { type T }
class C2CovCov[+A, +B] { type T }
class C2CovCon[+A, -B] { type T }
class C2ConInv[-A, B] { type T }
class C2ConCov[-A, +B] { type T }
class C2ConCon[-A, -B] { type T }

@main def Test =
  println(classVariances[C1Inv])
  println(classVariances[C1Cov])
  println(classVariances[C1Con])
  println(classVariances[C2InvInv])
  println(classVariances[C2InvCov])
  println(classVariances[C2InvCon])
  println(classVariances[C2CovInv])
  println(classVariances[C2CovCov])
  println(classVariances[C2CovCon])
  println(classVariances[C2ConInv])
  println(classVariances[C2ConCov])
  println(classVariances[C2ConCon])
