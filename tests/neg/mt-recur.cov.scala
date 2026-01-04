// like mt-recur.scala, but covariant
class Cov[+T]

type Recur[X] = X match // error
  case Int => Cov[Recur[X]]

def x = ??? : Recur[Int]
