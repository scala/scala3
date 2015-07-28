object mutual_specialization {
  class A[T] {
    def foo[@specialized(Double) U](b: U, n: Int): Unit = if (n > 0) bar(b, n-1)
    def bar[@specialized(Double) V](a: V, n: Int): Unit = if (n > 0) foo(a, n-1)
  }
}
