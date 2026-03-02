import annotation.retains
import language.experimental.erasedDefinitions

class CT[E <: Exception]
type CanThrow[E <: Exception] = CT[E] @retains[caps.any.type]
type Top  = Any @retains[caps.any.type]

infix type throws[R, E <: Exception] = (erased CanThrow[E]) ?=> R

class Fail extends Exception

def raise[E <: Exception](e: E): Nothing throws E = throw e

def foo(x: Boolean): Int throws Fail =
  if x then 1 else raise(Fail())

def handle[E <: Exception,  R <: Top](op: (CT[E] @retains[caps.any.type]) => R)(handler: E => R): R =
  val x: CT[E] = ???
  try op(x)
  catch case ex: E => handler(ex)

def test: Unit =
  val b = handle[Exception, () => Nothing] {
    (x: CanThrow[Exception]) => () => raise(new Exception)(using x)  // error
  } {
    (ex: Exception) => ???
  }
