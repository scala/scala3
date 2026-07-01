// Companion to tests/neg/i21013.scala: a wildcard application is accepted
// when it reduces without the wildcard taking part, or when it stays stuck
// with every wildcarded parameter occurring only in the scrutinee.

type Single[X] = X match             // scrutinee only
  case Int => String

type WithDefault[X] = X match        // scrutinee only, with a default
  case Int => String
  case String => Int
  case _ => Long

type UnderCtor[X] = List[X] match    // scrutinee, under a constructor
  case List[Int] => String
  case _ => Long

type Mixed[X, Y] = X match           // only the scrutinee parameter is wildcarded
  case Int => (Y, Y)
  case _ => List[Y]

// K occurs in the bound, but PlainBound[?, Int] reduces to Nothing without K
// taking part; contrast the stuck InBound[?, ?] in tests/neg
type PlainBound[K, S] <: List[K] = S match
  case Int => Nothing

def Test: Unit =
  val a1: Single[?]          = ???
  val a2: WithDefault[?]     = ???
  val a3: UnderCtor[?]       = ???
  val a4: Mixed[?, Int]      = ???
  val a5: PlainBound[?, Int] = ???

  // a nested wildcard is an ordinary closed existential argument, not a
  // top-level wildcard, so the application is sound (cf. M[?] in neg)
  val a6: Single[List[?]]    = ???
  val a7: Mixed[List[?], Int] = ???

  val b1: Single[? <: Int] = ???                          // bounded wildcards
  val b2: WithDefault[? >: Nothing <: Int | String] = ???

  val c1: List[Single[?]] = Nil                           // nested in a constructor
  val c2: Option[WithDefault[?]] = None
