package c

import b.B

object C {
  @main def run = {
    assert(B.transparentPower(2.0, 2) == 4.0)
    assert(B.transparentPower(2.0, 3) == 8.0)
    assert(B.transparentPower(2.0, 4) == 16.0)
  }
}
