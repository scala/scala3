import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  new Foo().bazApply(a = "abc", b = 1)  // error
}
