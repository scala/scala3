package foo

import scala.annotation.binaryAPI

class A(@binaryAPI private[A] val p: Int):
  @binaryAPI private[A] val a: Int = 1
  @binaryAPI private[A] lazy val b: Int = 1
  @binaryAPI private[A] var c: Int = 1
  @binaryAPI private[A] def d: Int = 1
  @binaryAPI private[A] given e: Int = 1
  @binaryAPI private[A] given f(using Double): Int = 1


def test(a: A) =
  a.p // error
  a.a // error
  a.b // error
  a.c // error
  a.d // error
  a.e // error
  a.f(using 1.0) // error
