import language.experimental.erasedDefinitions

object scalax:
  erased class CanThrow[-E <: Exception]

  infix type raises[R, +E <: Exception] = CanThrow[E] ?=> R

  class Fail extends Exception

  def raise[E <: Exception](e: E): Nothing raises E = throw e

import scalax._

def foo(x: Boolean): Int raises Fail =
  if x then 1 else raise(Fail())

def bar(x: Boolean)(using CanThrow[Fail]): Int = foo(x)
def baz: Int raises Exception = foo(false)

@main def Test =
  try
    given CanThrow[Fail] = ???
    println(foo(true))
    println(foo(false))
  catch case ex: Fail =>
    println("failed")
  try
    given CanThrow[Exception] = ???
    println(baz)
  catch case ex: Fail =>
    println("failed")
