import language.experimental.erasedDefinitions

class CT[E <: Exception]
type CanThrow[E <: Exception] = CT[E] @retains(*)
type Top  = Any @retains(*)

infix type throws[R, E <: Exception] = (erased CanThrow[E]) ?=> R

class Fail extends Exception

def raise[E <: Exception](e: E): Nothing throws E = throw e

def foo(x: Boolean): Int throws Fail =
  if x then 1 else raise(Fail())

def handle[E <: Exception, R <: Top](op: (CanThrow[E]) => R)(handler: E => R): R =
  val x: CanThrow[E] = ???
  try op(x)
  catch case ex: E => handler(ex)

def test: Unit =
  val b = handle[Exception, () => Nothing] {
    (x: CanThrow[Exception]) => () => raise(new Exception)(using x)
  } { // error
    (ex: Exception) => ???
  }
