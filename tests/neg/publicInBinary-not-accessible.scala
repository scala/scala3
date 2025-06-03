//> using options -Werror -WunstableInlineAccessors

package foo

import scala.annotation.publicInBinary

class A(@publicInBinary private[A] val p: Int):
  @publicInBinary private[A] val a: Int = 1
  @publicInBinary private[A] lazy val b: Int = 1
  @publicInBinary private[A] var c: Int = 1
  @publicInBinary private[A] def d: Int = 1
  @publicInBinary private[A] given e: Int = 1
  @publicInBinary private[A] given f(using Double): Int = 1

def test(a: A) =
  a.p // error
  a.a // error
  a.b // error
  a.c // error
  a.d // error
  a.e // error
  a.f(using 1.0) // error
