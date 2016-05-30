import scala.language.dynamics

class Foo extends scala.Dynamic {
  def updateDynamic(name: String)(value: Int): Unit = ???
}

object DynamicTest {
  def test: Int = new Foo().bazUpdate = 42 // error
}
