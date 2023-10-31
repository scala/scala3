

@FunctionalInterface
abstract class Q[A] {
  def apply(a: A): Int
}

class C extends Q[?]  // error: Type argument must be fully defined

object O {
  def m(i: Int): Int = i
  val x: Q[?] = m
}
