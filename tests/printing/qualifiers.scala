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
  val x6: Int = id[{x: Int with x < 0}](1) + id[Neg](-1)

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
