import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamicNamed(name: String)(args: (String, Int)*): String = ???
}

object DynamicTest {
  new Foo().applyDynamic("bar")("1" -> 2) // error
}
