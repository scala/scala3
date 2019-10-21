class Co[+A] {
  def foo(a: A): Unit = {} // error: contravariant occurs in covariant position
  def bar: A = ???
}
