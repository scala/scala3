//> using options -Werror

import scala.language.`future-migration`

class Foo:
  private[this] def foo: Int = ??? // error: migration warning
  protected[this] def bar: Int = ??? // error: migration warning
