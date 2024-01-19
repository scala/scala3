import scala.language.`future-migration`

class Foo:
  private[this] def foo: Int = ??? // warn
  protected[this] def bar: Int = ??? // warn
