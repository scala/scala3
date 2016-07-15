import scala.language.dynamics

class Foo extends scala.Dynamic {
  def selectDynamic(name: Int): String = ???
}

object DynamicTest {
  def testSelect: String = {
    new Foo().bazSelect // error
  }
}
