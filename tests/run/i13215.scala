//> using options -Werror -WunstableInlineAccessors

import scala.language.experimental.mode
import scala.annotation.publicInBinary

package foo {
  trait Bar:
    inline def baz = Baz

  @publicInBinary private[foo] object Baz
}

@main def Test: Unit =
  val bar = new foo.Bar {}
  bar.baz
