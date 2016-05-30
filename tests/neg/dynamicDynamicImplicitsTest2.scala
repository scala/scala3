import scala.language.dynamics

class Foo extends scala.Dynamic {
  def applyDynamic(name: String)(args: Any*): String = ???
}

object DynamicTest {
  implicit class Bar(foo: Foo) {
    def bazApply: Int = ???
  }

  def baz: String = new Foo().bazApply("") // error
}
