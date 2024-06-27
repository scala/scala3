//> using options -deprecation
import scala.language.`future-migration`

class Foo:
  private[this] def foo: Int = ??? // warn: deprecation warning
  protected[this] def bar: Int = ??? // warn: deprecation warning
