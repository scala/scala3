import language.experimental.erasedTerms

object scalax:
  erased class CanThrow[E <: Exception]

  infix type throws[R, E <: Exception] = CanThrow[E] ?=> R

  class Fail extends Exception

  def raise[E <: Exception](e: E): Nothing throws E = throw e

import scalax._

def foo(x: Boolean): Int throws Fail =
  if x then 1 else raise(Fail())

def bar(x: Boolean)(using CanThrow[Fail]): Int =
  if x then 1 else raise(Fail())

@main def Test =
  try
    given CanThrow[Fail] = ???
    println(foo(true))
    println(foo(false))
  catch case ex: Fail =>
    println("failed")
