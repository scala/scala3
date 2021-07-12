import language.`future-migration`
class Test
object Test {
  def foo[A <% Test](x: A) = x  // error
}
