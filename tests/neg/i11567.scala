import language.future
class Test
object Test {
  def foo[A <% Test](x: A) = x  // error
}
