trait A
trait B
type M[X] = X match
  case A => Int
  case B => String
val x: String = ??? : M[B]

