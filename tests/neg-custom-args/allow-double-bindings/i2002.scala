class Test {
  def foo(i: Int): Int = i
  def foo(implicit i: Int): Int = i // error
}
