// #21013: a match type alias applied to a wildcard is rejected when the
// application does not reduce and the wildcarded parameter occurs outside the
// scrutinee -- in a case pattern, a case body, or the declared upper bound.
// Accepted applications are in tests/pos/i21013.scala.
// The body/bound aliases take an extra scrutinee parameter `S` so that the
// alias itself stays a match type instead of reducing away at definition.

type InPattern[K] = Double match
  case K => Int                      // K in a pattern

type InNestedPattern[K] = Double match
  case List[K] => Int                // K in a nested pattern

type InBody[K, S] = S match
  case Int => List[K]                // K in a body

type InBodyTwice[K, S] = S match
  case Int => (K, K)                 // K twice in a body

// K in the bound of an application that stays stuck (wildcard scrutinee);
// contrast PlainBound[?, Int] in tests/pos, which reduces and is accepted
type InBound[K, S] <: List[K] = S match
  case Int => Nothing

// bound that is itself a match alias: InAliasBound[?] <: Bad[?], where Bad[?]
// would reduce unsoundly -- the case that makes bound occurrences dangerous
type Bad[K] = Double match
  case K => Int
type InAliasBound[K, S] <: Bad[K] = S match
  case Int => Nothing

def Test: Unit =
  val a1: InPattern[?]         = ??? // error
  val a2: InNestedPattern[?]   = ??? // error
  val a3: InBody[?, Int]       = ??? // error
  val a4: InBodyTwice[?, Int]  = ??? // error
  val a5: InBound[?, ?]        = ??? // error
  val a6: InAliasBound[?, Int] = ??? // error
