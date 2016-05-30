import scala.language.dynamics

class Foo extends scala.Dynamic {
  def selectDynamic(name: String): Array[String] = ???
  def applyDynamic(name: String)(args: Any*): String = ???
}

object DynamicTest {
  new Foo().bazApply(a = "abc", b = 1)  // error
}
