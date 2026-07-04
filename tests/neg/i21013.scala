// #21013: a wildcard application of a match type alias is rejected when it
// does not reduce and the wildcarded parameter occurs outside the scrutinee.
// Accepted applications are in tests/pos/i21013.scala.

type InPattern[K] = Double match
  case K => Int

type InNestedPattern[K] = Double match
  case List[K] => Int

// The pattern shape that would genuinely reduce with the wildcard taking
// part: List[Double] matches List[?], so InMatchingPattern[?] would reduce
// to Int even though InMatchingPattern[String] does not reduce.
type InMatchingPattern[K] = List[Double] match
  case List[K] => Int

type InBody[K, S] = S match
  case Int => List[K]

type InBodyTwice[K, S] = S match
  case Int => (K, K)

type InBound[K, S] <: List[K] = S match
  case Int => Nothing

type Bad[K] = Double match
  case K => Int
type InAliasBound[K, S] <: Bad[K] = S match
  case Int => Nothing

def Test: Unit =
  val a1: InPattern[?]          = ??? // error
  val a2: InNestedPattern[?]    = ??? // error
  val a3: InMatchingPattern[?]  = ??? // error
  val a4: InBody[?, Int]        = ??? // error
  val a5: InBodyTwice[?, Int]   = ??? // error
  val a6: InBound[?, ?]         = ??? // error
  val a7: InAliasBound[?, Int]  = ??? // error
