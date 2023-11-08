import scala.language.future

class Foo:
  private[this] def foo: Int = ??? // error
  protected[this] def bar: Int = ??? // error
