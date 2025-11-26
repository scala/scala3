package app

import Macros.*
import A.A

object App {
  @main def hasFields(expected: Boolean): Unit = {
    val actual = Macros.hasAnyField[A]
    assert(expected == actual, s"Expected $expected, obtained $actual")
  }
}
