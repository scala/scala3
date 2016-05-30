import scala.language.dynamics

class Foo extends scala.Dynamic {
  def selectDynamic(name: String): String = ???
}

object DynamicTest {
  def testSelect: Int = {
    new Foo().bazSelect // error
  }
}
