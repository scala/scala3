import scala.language.dynamics

class Foo extends scala.Dynamic {
  def selectDynamic(name: String): String = ???
  def applyDynamicNamed(name: String)(args: Any*): String = ???
}

object DynamicTest {
  new Foo().bazApply() // error
}
