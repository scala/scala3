import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamic(name: String)(args: String*): String = ???
}

object DynamicTest {
  new Foo().bazApply(1, 2, 3) // error // error // error
}
