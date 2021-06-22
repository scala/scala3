import language.experimental.erasedDefinitions
import annotation.ability
import java.io.IOException

class CanThrow[E] extends Retains[*]
type Top  = Any retains *
infix type ==> [A, B] = (A => B) retains *
class OtherCap extends Retains[*]

def handle[E <: Exception, T <: Top](op: (CanThrow[E] ?=> T))(handler: (E => T) retains T): T =
  val x: CanThrow[E] = ???
  try op(using x)
  catch case ex: E => handler(ex)

def raise[E <: Exception](ex: E)(using CanThrow[E]): Nothing =
  throw ex

def test2: Int =
  def f(c: OtherCap, a: Boolean): Boolean => CanThrow[IOException] ?=> Int =
    handle {
      if !a then raise(IOException())
      (b: Boolean) => (_: CanThrow[IOException]) ?=>
        if !b then raise(IOException())
        1
    } {
      ex => (b: Boolean) => (_: CanThrow[IOException]) ?=> -1
    }
  handle {
    val c = OtherCap()
    val g = f(c, true)
    g(false)
    f(c, true)(false)
  } {
    ex => -1
  }
