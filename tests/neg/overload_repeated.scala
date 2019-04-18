object Test {
  def foo(a: Any) = a
  def foo(s: String*) = s
  foo("") // error
}
