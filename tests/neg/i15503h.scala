// scalac: -Xfatal-warnings -Wunused:linted

import collection.mutable.Set // error

class A {
  private val a = 1 // error
  val b = 2 // OK

  private def c = 2 // error
  def d(using x:Int): Int = b // ok
  def e(x: Int) = 1 // OK
  def f =
    val x = 1 // error
    def f = 2 // error
    3

  def g(x: Int): Int = x match
    case x:1 => 0 // OK
    case _ => 1
}