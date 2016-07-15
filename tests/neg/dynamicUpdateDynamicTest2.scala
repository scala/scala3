import scala.language.dynamics

class Foo extends scala.Dynamic {
  def updateDynamic(name: String)(value: String): Unit = ???
}

object DynamicTest {
  new Foo().bazUpdate = 42 // error
}
