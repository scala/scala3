import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamic(name: String)(args: Any*): String = ???
}

object DynamicTest {
  new Foo().applyDynamicNamed("abc")() // error
}
