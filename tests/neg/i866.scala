object Test {
  def x: Int = ""     // error
}
import nonexistent._  // error; this one will swallow all errors below.
object Foo {
  def bar(implicit x: NonExistent) = ???
  val baz = bar
}

