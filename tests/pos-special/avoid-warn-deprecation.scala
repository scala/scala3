object A {
  @deprecated("use bar instead of this one", "0.2.3")
  def foo: Int = 3
}

object B {
  A.foo
}
