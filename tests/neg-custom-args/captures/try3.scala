import java.io.IOException

class CT[E]
type CanThrow[E] = CT[E]^
type Top  = Any^

def handle[E <: Exception, T <: Top](op: (lcap: caps.Capability) ?-> CT[E]^{lcap} ?=> T)(handler: E => T): T =
  val x: CT[E] = ???
  try op(using caps.any)(using x)
  catch case ex: E => handler(ex)

def raise[E <: Exception](ex: E)(using CanThrow[E]): Nothing =
  throw ex

@main def Test: Int =
  def f(a: Boolean) =
    handle {  // error: implementation restriction: curried dependent CFT not supported
              // should work but give capture error
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
