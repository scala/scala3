package b

import a.*

// Add a separate file in the same project as A, second compile via Zinc
object B {
  transparent inline def caller = A.foo("abc", 2)
}
