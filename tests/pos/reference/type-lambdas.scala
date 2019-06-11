package typeLambdas

object Test {

  type T =  [+X, Y] =>> Map[Y, X]

  type CTL = [X] =>> [Y] =>> (X, Y)
  type T3 = CTL[Int][String]

  type T2[+X <: X => X] = Any  // OK - variance is not checked in param bounds
}
