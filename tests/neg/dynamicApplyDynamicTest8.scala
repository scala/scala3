import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamicNamed(name: String)(args: (String, Int)*): String = ???
}

object DynamicTest {
  def test: String = new Foo().bazApply("1" -> 2)  // error
}
