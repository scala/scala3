class Test {
  def foo(x: Pure) = ???
  def foo(x: Impure) = ??? // error

  def foo2(a: Int)(x: Pure) = ???
  def foo2(a: Int) = ??? // error
}
