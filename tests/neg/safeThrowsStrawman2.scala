import language.experimental.erasedDefinitions

object scalax:
  erased class CanThrow[E <: Exception]
  type CTF = CanThrow[Fail]

  infix type raises[R, E <: Exception] = CanThrow[E] ?=> R

  class Fail extends Exception

  def raise[E <: Exception](e: E): Nothing raises E = throw e

import scalax._

def foo(x: Boolean, y: CanThrow[Fail]): Int raises Fail =
  if x then 1 else raise(Fail())

def bar(x: Boolean)(using CanThrow[Fail]): Int =
  if x then 1 else raise(Fail())

@main def Test =
  try
    given ctf: CanThrow[Fail] = ???
    val x = new CanThrow[Fail]()      // OK, x is erased
    val y: Any = new CanThrow[Fail]() // error: illegal reference to erased class CanThrow
    val y2: Any = new CTF()       // error: illegal reference to erased class CanThrow
    println(foo(true, ctf))       // error: ctf is declared as erased, but is in fact used
    val a = (1, new CanThrow[Fail]()) // error: illegal reference to erased class CanThrow
    def b: (Int, CanThrow[Fail]) = ???
    def c = b._2                  // ok; we only check creation sites
    bar(true)(using ctf)
  catch case ex: Fail =>
    println("failed")
