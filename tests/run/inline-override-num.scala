trait Num[T] {
  def plus(x: T, y: T): T
}

object Num {
  class IntNum extends Num[Int] {
    inline def plus(x: Int, y: Int): Int = x + y
  }
  given IntNum()

  extension [T](inline x: T)(using inline num: Num[T]) {
    inline def +(inline y: T): T = num.plus(x, y)
  }
}

import Num.+

inline def twiceInlined[T: Num](x : T): T = x + x
def twice[T: Num](x : T): T = x + x

@main def Test =
  val x: Int = 3
  assert(6 == twiceInlined(x)) // code will be x + x using the primitive Int.+
  assert(6 == twice(x)) // will call IntNum.plus through virtual dispatch
