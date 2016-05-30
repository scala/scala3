import scala.language.dynamics

class Foo extends scala.Dynamic {
  def selectDynamic(name: String): String = ???
}

object DynamicTest {
  implicit class Bar(foo: Foo) {
    def bazSelect: Int = ???
  }

  def baz: String = new Foo().bazSelect // error
}
