// Companion to tests/neg/i21013.scala: wildcard applications of match
// type aliases are sound, and must be accepted, when every wildcarded
// parameter occurs only in the scrutinee of the underlying match type.

// Sound: parameter only in scrutinee (single case)
type Ok1[X] = X match
  case Int => String

val a1: Ok1[?] = ???
def b1: Ok1[?] = ???
val c1: List[Ok1[?]] = Nil
val d1: Option[Ok1[?]] = None

// Sound: parameter only in scrutinee (multiple cases, default)
type Ok2[X] = X match
  case Int => String
  case String => Int
  case _ => Double

val a2: Ok2[?] = ???
def b2: Array[Ok2[?]] = null

// Sound mix: the wildcarded parameter only appears in the scrutinee,
// the other parameter is given concretely and may appear anywhere.
type Ok3[X, Y] = X match
  case Int => (Y, Y)
  case String => List[Y]

val a3: Ok3[?, Int] = ???
def b3: Ok3[?, String] = ???

// Bounded wildcard, still sound
def b4: Ok1[? <: Int] = ???
def b5: Ok2[? >: Nothing <: Int | String] = ???
