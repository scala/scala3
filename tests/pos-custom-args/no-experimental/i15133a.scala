import scala.annotation.experimental

@experimental
inline def foo: Any = ???

@experimental
object Test {
  val x = foo

  def bar() = {
    foo
  }

  foo
}
