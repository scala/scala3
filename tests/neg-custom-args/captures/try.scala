import language.experimental.erasedDefinitions

class CT[E <: Exception]
type CanThrow[E <: Exception] = CT[E] @retains(*)
type Top  = Any @retains(*)

infix type throws[R, E <: Exception] = (erased CanThrow[E]) ?=> R

class Fail extends Exception

def raise[E <: Exception](e: E): Nothing throws E = throw e

def foo(x: Boolean): Int throws Fail =
  if x then 1 else raise(Fail())

def handle[E <: Exception, R <: Top](op: CanThrow[E] => R)(handler: E => R): R =
  val x: CanThrow[E] = ???
  try op(x)
  catch case ex: E => handler(ex)

def test =
  val a = handle[Exception, CanThrow[Exception]] { // error
    (x: CanThrow[Exception]) => x
  }{
    (ex: Exception) => ???
  }

  val b = handle[Exception, () -> Nothing] {  // error
    (x: CanThrow[Exception]) => () => raise(new Exception)(using x)
  } {
    (ex: Exception) => ???
  }

  val xx = handle {
    (x: CanThrow[Exception]) =>
      () =>
        raise(new Exception)(using x)
        22
  } {  // error
    (ex: Exception) => () => 22
  }
  val yy = xx :: Nil
  yy // OK


val global = handle {
  (x: CanThrow[Exception]) =>
    () =>
      raise(new Exception)(using x)
      22
} {  // error
  (ex: Exception) => () => 22
}