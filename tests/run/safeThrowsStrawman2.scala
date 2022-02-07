import language.experimental.erasedDefinitions

object scalax:
  erased class CanThrow[-E <: Exception]

  infix type raises[R, +E <: Exception] = CanThrow[E] ?=> R

  class Fail extends Exception

  def raise[E <: Exception](e: E): Nothing raises E = throw e

  private class Result[T]:
    var value: T = scala.compiletime.uninitialized

  def try1[R, E <: Exception](body: => R raises E)(c: E => Unit): R =
    try2(body)(c) {}

  def try2[R, E <: Exception](body: => R raises E)(c: E => Unit)(f: => Unit): R =
    val res = new Result[R]
    try
      given CanThrow[E] = new CanThrow
      res.value = body
    catch c.asInstanceOf[Throwable => Unit]
    finally f
    res.value

  extension [R, E <: Exception](t: (E => Unit) => R) def catch1(c: E => Unit) = t(c)

  extension [R, E <: Exception](c: ( => Unit) => R) def finally1(f: => Unit) = c(f)

import scalax._

def foo(x: Boolean): Int raises Fail =
  if x then 1 else raise(Fail())

def bar(x: Boolean)(using CanThrow[Fail]): Int = foo(x)
def baz: Int raises Exception = foo(false)

@main def Test =
  try1 {
    println(foo(true))
    println(foo(false))
  } catch1 {
    case ex: Fail =>
      println("failed")
  }
  try2 {
    println(baz)
  } catch1 {
    case ex: Fail =>
      println("failed")
  } finally1 {
    println(2)
  }
