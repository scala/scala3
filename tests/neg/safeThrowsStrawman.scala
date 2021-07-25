import language.experimental.erasedDefinitions
import annotation.implicitNotFound

object scalax:
  @implicitNotFound("The capability to throw exception ${E} is missing.\nThe capability can be provided by one of the following:\n - A using clause `(using CanThrow[${E}])`\n - A raises clause in a result type such as `X raises ${E}`\n - an enclosing `try` that catches ${E}")
  erased class CanThrow[-E <: Exception]

  infix type raises[R, +E <: Exception] = CanThrow[E] ?=> R

  class Fail extends Exception

  def raise[E <: Exception](e: E): Nothing raises E = throw e

import scalax._

def foo(x: Boolean): Int =
  if x then 1 else raise(Fail())  // error

def bar: Int raises Exception =
  raise(Fail())

@main def Test =
  try
    erased given CanThrow[Fail] = ???
    println(foo(true))
    println(foo(false))
    println(bar)        // error
  catch case ex: Fail =>
    println("failed")
