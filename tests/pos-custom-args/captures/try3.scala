import language.experimental.erasedDefinitions
import java.io.IOException

class CanThrow[-E] extends caps.SharedCapability

def handle[E <: Exception, T](op: CanThrow[E] ?=> T)(handler: E => T): T =
  val x: CanThrow[E] = ???
  try op(using x)
  catch case ex: E => handler(ex)

def raise[E <: Exception](ex: E)(using CanThrow[E]): Nothing =
  throw ex

def test1: Int =
  def f(a: Boolean)  =
    handle {
      if !a then raise(IOException())
      (b: Boolean) => (_: CanThrow[IOException]) ?=>
        if !b then raise(IOException())
        0
    } {
      ex => (b: Boolean) => (_: CanThrow[IOException]) ?=> -1
    }
  def fc(a: Boolean): Boolean -> CanThrow[IOException] ?-> Int = f(a)
  handle {
    val g = f(true)
    g(false) // can raise an exception
    f(true)(false) // can raise an exception
  } {
    ex => -1
  }
/*
def test2: Int =
  def f(a: Boolean): Boolean => CanThrow[IOException] ?=> Int =
    handle { // error
      if !a then raise(IOException())
      (b: Boolean) =>
        if !b then raise(IOException())
        0
    } {
      ex => (b: Boolean) => -1
    }
  handle {
    val g = f(true)
    g(false) // would raise an uncaught exception
    f(true)(false) // would raise an uncaught exception
  } {
    ex => -1
  }
*/