package app

import Macros.*
import A.B

object App {
  @main def hasFields(expected: Boolean): Unit = {
    val actual = Macros.hasAnyField[B]
    assert(expected == actual, s"Expected $expected, obtained $actual")
  }
}
