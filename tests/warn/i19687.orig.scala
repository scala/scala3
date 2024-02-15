enum Eq[A, B]:
  case Refl[A]() extends Eq[A, A]

enum T[A]:
  case S(witness: Eq[A, String])
  case I(witness: Eq[A, Int])

def partial(tagged: T[Int]) : Int =
  tagged match
    case T.I(Eq.Refl()) => 0
