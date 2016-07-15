import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  new Foo().bazUpdate = 42 // error
}
