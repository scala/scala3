
trait A
trait B

type M[X] = X match
  case A => Int
  case B => String

val x: String = ??? : M[B] // error

type MB = M[B]
val x2: String = ??? : MB // error
