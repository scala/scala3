import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  new Foo().bazSelectUpdate(6) = "a" // error
}
