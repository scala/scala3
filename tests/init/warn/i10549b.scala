class Wrap {
  def qux[T](e: E[T]) = e.foo

  abstract class E[+T] { def foo: T }
  object E {
    final val A: E[Nothing] = new E { def foo = ref }
    val ref = qux(A)      // warn
  }
}
