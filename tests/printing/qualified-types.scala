package example

type Neg = {x: Int with x < 0}
type Pos = {x: Int with x > 0}
type Pos2 = {x: Int
  with x > 0
}
type Pos3 = {x: Int with
  x > 0
}
type Pos4 =
  {x: Int with x > 0}
type Pos5 = {x: Int with
  val res = x > 0
  res
}

type UninhabitedInt = Int with false

type Nested = {x: Int with { val y: {z: Int with z > 0} = ??? ; x > y }}
type Intersection = Int & {x: Int with x > 0}
type ValRefinement = {val x: Int with x > 0}

def id[T](x: T): T = x

def test() =
  val x: Pos = 1
  val x2: {x: Int with x > 0} = 1
  val x3: {
    x: Int with x > 0
  } = 1
  val x4: {x: Int with
    x > 0
  } = 1
  val x5: Int with x > 0 = 1
  val x6: Int = id[{x: Int with x < 0}](-1) + id[Neg](-1)

// `val x: Int with x > 0` is desugared to `val x: {x: Int with x > 0}`: if the
// name of a qualifier argument is not specified, it is assumed to be the same
// as the parent `val` definition.
def implicitArgumentName() =
  val x0: (Int with x0 > 0) | (String with x0 == "foo") = ???
  val x1: Int with x1 > 0 = ???
  val x2: (Int with x2 > 0) = ???
  val x3: (Int with x3 > 0) & (Int with x3 < 10) = ???
  val x4: (Int with x4 > 0) & Int with x4 < 10 = ???
  val x5: Int & String with false = ???
  val x6: ((Int with x6 > 0) & Int) with x5 < 10 = ???
  val x7: (Int with x7 > 0) with x6 < 10 = ???
  val x8: ((Int with x8 > 0) with x7 < 10) = ???

  val x9: Any = 42
  x9 match
    case y: Int with y > 0 =>
      println(s"$y is positive")
    case _ => ()

  (42, 42) match
    case (y: Int with y > 0, z: Int with z > 0) =>
      println(s"$y and $z are both positive")
    case _ => ()

def bar(x: Int with x > 0) = ???
def secondGreater1(x: Int, y: Int)(z: {w: Int with x > y}) = ???
def secondGreater2(x: Int, y: Int)(z: Int with x > y) = ???

class Foo:
  val x: Int with x > 0 = 1

trait A
// Not a qualified type:
given A with
  val b = false
  id(true)

// Also not qualified types:
type T1 = {val x: Int}
type T2 = {
  val x: Int
}
type T3 = {type T = Int}
type T4 = {def x: Int}
type T5 = {var x: Int}
type T6 = Object {val x: Int}
type T7 = Object:
  val x: Int
