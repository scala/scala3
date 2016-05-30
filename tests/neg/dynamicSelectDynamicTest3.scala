import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  new Foo().bazSelect // error
}
