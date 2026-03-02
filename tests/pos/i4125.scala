import scala.language.experimental.erasedDefinitions

object Test {
  def foo: ((erased x: Int, y: Int) => Int) = (erased x, y) => 1
  def bar: ((erased x: Int, y: Int) ?=> Int) = (erased x, y) ?=> 1
}
