// Issue #21013: applying a match type alias to a wildcard argument is
// unsound when the type parameter appears outside the match scrutinee
// (in a pattern, a case body, or the declared upper bound).

type M1[K] = Double match
  case K => Int // K appears in a pattern

type M2[K] = Double match
  case Option[K] => Int // K appears in a pattern

type M3[X, Y] = X match
  case Int => (Y, Y) // Y appears in a case body

type M4[K, S] <: List[K] = S match // K appears in the declared upper bound
  case Int => List[K]

def Test: Unit =
  val x1: M1[?] = ??? // error
  val x2: M2[?] = ??? // error
  val x3: M3[Int, ?] = ??? // error
  val x4: M4[?, String] = ??? // error

  // The unsoundness observed in #21013: M1[?] should not be a valid
  // supertype of M1[Int]. With the fix, M1[?] itself is rejected.
  // (If it were accepted, M1[?] would reduce to Int, but M1[Int] would not.)
