

import scala.annotation.experimental

object Macro {
  @experimental
  inline def foo() = fooImpl

  @experimental
  private def fooImpl = ()
}