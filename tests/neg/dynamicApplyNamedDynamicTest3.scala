import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  new Foo().bazApply("abc", b = 1) // error
}
