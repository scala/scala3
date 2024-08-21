//> using options -experimental -Werror -WunstableInlineAccessors

import scala.annotation.publicInBinary

package foo {
  trait Bar:
    inline def baz = Baz

  @publicInBinary private[foo] object Baz
}

@main def Test: Unit =
  val bar = new foo.Bar {}
  bar.baz
