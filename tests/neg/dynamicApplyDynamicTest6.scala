import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamic(name: String)(args: String*): String = ???
}

object DynamicTest {
  def test: Int = new Foo().bazApply() // error
}
