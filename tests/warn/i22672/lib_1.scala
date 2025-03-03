
package p

import annotation.*

@deprecated("old api", since="1.0")
def g = 42

//@deprecated("new api", since="1.0")
@nowarn("cat=deprecation")
inline def f =
  g

transparent inline def body =
  g: @nowarn
