package canThrowStrawman
import language.experimental.erasedTerms

class CanThrow[E <: Throwable]

infix type throws1[R, E <: Throwable] = (erased CanThrow[E]) ?=> R

class Fail extends Exception

def foo(x: Boolean): Int throws1 Fail =
  if x then 1 else throw Fail()
