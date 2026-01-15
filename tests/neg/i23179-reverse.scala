// Test that reverse cases of #23179 are properly rejected by the type checker.
// The original issue was about SAM expecting Array[T] but impl having Array[? >: T].
// Due to array invariance, the reverse cases cannot occur.

object TestReverse {
  trait A { def f(a: Array[? <: AnyRef]): Any }
  def g(a: A) = a.f(Array.empty[AnyRef])

  def test(): Unit = {
    g((x: Array[AnyRef]) => x.headOption) // error: Array[AnyRef] is not a subtype of Array[? <: AnyRef]
  }
}

object TestResult {
  trait A { def f(a: Int): Array[AnyRef] }
  def g(a: A) = a.f(1)

  def test(): Unit = {
    val arr: Array[? >: AnyRef] = Array("hello")
    g((x: Int) => arr) // error: Array[? >: AnyRef] is not a subtype of Array[AnyRef]
  }
}
