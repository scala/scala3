import scala.language.`future-migration`

class Foo:
  private[this] def foo: Int = ??? // warn: migration warning
  protected[this] def bar: Int = ??? // warn: migration warning
