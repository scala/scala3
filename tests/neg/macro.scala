class A {
  def bla = ???
  def foo: Int = macro bla // error
}
