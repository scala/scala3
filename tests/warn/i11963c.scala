

object Test {
  def foo: Any = {
    open class Bar // warn
    new Bar
  }
}
