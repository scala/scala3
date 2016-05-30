import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  new Foo().bazApply("abc", 1)  // error
}
