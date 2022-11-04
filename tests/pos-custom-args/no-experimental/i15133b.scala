import scala.annotation.experimental

@experimental
inline def foo: Any = ???

object Test {
  @experimental val x = foo

  @experimental
  def bar() = {
    foo
  }
}
