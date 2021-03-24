import language.experimental.erasedDefinitions

object scalax:
  erased class CanThrow[-E <: Exception]

  infix type throws[R, +E <: Exception] = CanThrow[E] ?=> R

  class Fail extends Exception

  def raise[E <: Exception](e: E): Nothing throws E = throw e

  private class Result[T]:
    var value: T = scala.compiletime.uninitialized

  def try1[R, E <: Exception](body: => R throws E): (E => Unit) => R = { c =>
    val res = new Result[R]
    try
      given CanThrow[E] = ???
      res.value = body
    catch c.asInstanceOf[Throwable => Unit]
    res.value
  }

  extension [R, E <: Exception](t: (E => Unit) => R) def catch1(c: E => Unit) = t(c)

import scalax._

def foo(x: Boolean): Int throws Fail =
  if x then 1 else raise(Fail())

def bar(x: Boolean)(using CanThrow[Fail]): Int = foo(x)
def baz: Int throws Exception = foo(false)

@main def Test =
  try1 {
    println(foo(true))
    println(foo(false))
  } catch1 {
    case ex: Fail =>
      println("failed")
  }
  try1 {
    println(baz)
  } catch1 {
    case ex: Fail =>
      println("failed")
  }
