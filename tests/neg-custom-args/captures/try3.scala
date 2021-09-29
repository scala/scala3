import java.io.IOException

class CT[E]
type CanThrow[E] = {*} CT[E]
type Top  = {*} Any

def handle[E <: Exception, T <: Top](op: CanThrow[E] ?=> T)(handler: E => T): T =
  val x: CanThrow[E] = ???
  try op(using x)
  catch case ex: E => handler(ex)

def raise[E <: Exception](ex: E)(using CanThrow[E]): Nothing =
  throw ex

@main def Test: Int =
  def f(a: Boolean) =
    handle { // error
      if !a then raise(IOException())
      (b: Boolean) =>
        if !b then raise(IOException())
        0
    } {
      ex => (b: Boolean) => -1
    }
  val g = f(true)
  g(false) // would raise an uncaught exception
  f(true)(false) // would raise an uncaught exception
