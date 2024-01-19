//> using options  -Wunused:linted

import collection.mutable.Set // warn

class A {
  private val a = 1 // warn
  val b = 2 // OK

  private def c = 2 // warn
  def d(using x:Int): Int = b // ok
  def e(x: Int) = 1 // OK
  def f =
    val x = 1 // warn
    def f = 2 // warn
    3

  def g(x: Int): Int = x match
    case x:1 => 0 // OK
    case _ => 1
}