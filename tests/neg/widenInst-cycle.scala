import scala.reflect.ClassTag

class Test {
  def foo[N >: C | D <: C, C, D](implicit ct: ClassTag[N]): Unit = {}
  // This used to lead to an infinite loop, because:
  // widenInferred(?C | ?D, ?N)
  // returns ?C, with the following extra constraints:
  // ?C := ?N
  // ?D := ?N
  // So we ended up trying to instantiate ?N with ?N.
  foo // error
}
