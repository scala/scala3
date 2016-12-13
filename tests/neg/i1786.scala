package scala

package object meta {
  def apply(x: Int): Int = x * x
}

class Test {
  def f(a: Any): Any = f(meta)         // error
  def g(a: Any): Any = f(scala.meta)   // error

  meta { 5 + 4 }

  scala.meta { 3 }

  val m1 = meta         // error
  val m2 = scala.meta   // error
}
