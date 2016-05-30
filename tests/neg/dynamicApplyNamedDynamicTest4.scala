import scala.language.dynamics

class Foo extends scala.Dynamic

object DynamicTest {
  new Foo().bazApply("abc", 4, b = 1, b = "bcd") // error
}
