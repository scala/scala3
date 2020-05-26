class Hi
class Lo extends Hi

object Test {
  def foo[T, U <: T](t: T, f: T => U): U = ???

  def test(hi: Hi, lo: Lo): Unit = {
    val ret = foo(hi, x => lo) // This used to infer U := Hi
    val y: Lo = ret
  }
}
