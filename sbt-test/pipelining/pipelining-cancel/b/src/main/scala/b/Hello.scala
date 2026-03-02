package b

import a.A

object Hello {
  @main def test(): Unit = {
    assert(A.foo == (1,2,3))
  }
}
