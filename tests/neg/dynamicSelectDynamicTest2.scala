import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  def testSelect = new Foo().bazSelect // error
}
