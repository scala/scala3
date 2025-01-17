
trait A
trait B

type M[X, Y] = Y match
  case A => Int
  case B => String

type Relevant[Z] = Z match
  case A => B
type NotRelevant[Z] = Z match
  case B => A

val x: M[NotRelevant[Nothing], Relevant[Nothing]] = 2 // error
