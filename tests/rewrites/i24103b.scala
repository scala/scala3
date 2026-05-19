object Test {
  def foo(x: Int): Int => Int = y => x + y
  def bar(x: Int): Int => Int = y => x * y

  def test(s: Int): Int = {
    (foo(1) _ compose bar(2))(s)
  }
}
