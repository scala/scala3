object mutual_specialization {
  class A[T] {
    def foo[T](b: B[T], n: Int): Unit = if (n > 0) b.bar(this, n-1)
  }
  class B[T] {
    def bar[T](a: A[T], n: Int): Unit = if (n > 0) a.foo(this, n-1)
  }
  def foobar[@specialized(Int, Float, Double) T](n: T): Unit = new A[T].foo(new B[T], 5)
  foobar(5)
}
