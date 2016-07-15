import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamicNamed(name: String)(args: Any*): String = ???
}

object DynamicTest {
  def test: Int = new Foo().bazApply("abc", 4, b = 1, b = "bcd") // error
}
