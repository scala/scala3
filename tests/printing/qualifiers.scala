package example

type Useless = {x: Int with true}
type Pos =
  {x: Int with x > 0}
type Neg = {x: Int with
  x < 0
}
type Nesting = {x: Int with { val y: {z: Int with z > 0} = ??? ; x > y }}
type Pos2 = Int & {x: Int with x > 0}
type ValRefinement = {val x: Int with x > 0}

def id[T](x: T): T = x

def test() =
  val x1: Pos = 1
  val x2: {x: Int with x > 0} = 1
  val x3: Int with x3 > 0 = 1
  val x4: Int = id[{x: Int with x < 0}](1) + id[Neg](-1)

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
type B = {val x: Int}
type C = Object {val x: Int}
