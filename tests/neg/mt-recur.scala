// an example of an infinite recursion match type
// using an _invariant_ type constructor
// see mt-recur.cov.scala for covariant
// used to track the behaviour of match type reduction
class Inv[T]

type Recur[X] = X match
  case Int => Inv[Recur[X]]

def x = ??? : Recur[Int] // error
