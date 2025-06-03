import scala.language.`3.4`

class Foo:
  private[this] def foo: Int = ??? // warn: migration warning
  protected[this] def bar: Int = ??? // warn: migration warning
