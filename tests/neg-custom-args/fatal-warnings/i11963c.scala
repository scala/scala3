object Test {
  def foo: Any = {
    open class Bar // error
    new Bar
  }
}
