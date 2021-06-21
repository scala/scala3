import language.experimental.erasedDefinitions
import annotation.ability
import java.io.IOException

class CanThrow[E] extends Retains[*]
type Top  = Any retains *
infix type ==> [A, B] = (A => B) retains *

def handle[E <: Exception, T <: Top](op: (CanThrow[E] ?=> T))(handler: (E => T) retains T): T =
  val x: CanThrow[E] = ???
  try op(using x)
  catch case ex: E => handler(ex)

def raise[E <: Exception](ex: E)(using CanThrow[E]): Nothing =
  throw ex

def test2: Int ==> Int =
  def f(a: Boolean): Boolean => CanThrow[IOException] ?=> Int ==> Int =
    handle {
      if !a then raise(IOException())
      (b: Boolean) => (_: CanThrow[IOException]) ?=>
        if !b then raise(IOException())
        (x: Int) => 1
    } {
      ex => (b: Boolean) => (_: CanThrow[IOException]) ?=> (x: Int) => -1
    }
  handle {  // error: inferred type argument ((Int => Int) retains *) is not allowed to capture the universal capability *
    val g = f(true)
    g(false) // would raise an uncaught exception
    f(true)(false) // would raise an uncaught exception
  } {
    ex => (x: Int) => -1
  }
