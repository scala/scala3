package tests
package optionalParams

class C(val a: Seq[String], val b: String = "", var c: String = "") //expected: class C(val a: Seq[String], val b: String = ..., var c: String = ...)
{
  def m(x: Int, s: String = "a"): Nothing //expected: def m(x: Int, s: String = ...): Nothing
    = ???
}

def f(x: Int, s: String = "a"): Nothing //expected: def f(x: Int, s: String = ...): Nothing
  = ???

extension (y: Int)
  def ext(x: Int = 0): Int //expected: def ext(x: Int = ...): Int
    = 0

def byname(s: => String = "a"): Int //expected: def byname(s: => String = ...): Int
  = 0

enum E(val x: Int = 0) //expected: enum E(val x: Int = ...)
{
  case E1(y: Int = 10) extends E(y) //expected: final case class E1(y: Int = ...) extends E
}
