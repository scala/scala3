
type M[F[_,_]] = Int match
  case 0 => String & M[F]

type M1 = M[[x,y] =>> x | y]
type M2 = M[[x,y] =>> x | y]

def Test: Unit =
  val x: M1 = ???
  val _: M2 = x // was error
