//> using options -Werror

import scala.language.`3.4`

class Foo:
  private[this] def foo: Int = ??? // error: migration warning
  protected[this] def bar: Int = ??? // error: migration warning
