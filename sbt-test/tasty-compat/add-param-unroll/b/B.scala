package b

import a.*

object B {
  transparent inline def caller = A.foo("abc", 2)
}
