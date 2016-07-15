import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamic(name: Int)(args: String*): String = ???
}

object DynamicTest {
  def test: String = new Foo().bazApply() // error
}
