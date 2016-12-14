package scala

package object meta {
  def apply(x: Int): Int = x * x
}

class Test {
  meta { 5 + 4  }

  scala.meta { 3 }

  scala.meta.`package` { 3 }

  // val m1 = meta         // error
  // val m2 = scala.meta   // error
  val m3 = scala.meta.`package`
  val m4 = meta.`package`
}
