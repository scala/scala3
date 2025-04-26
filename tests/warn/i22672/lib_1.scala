
package p

import annotation.{unchecked as _, *}

@deprecated("old api", since="1.0")
def g = 42

//@deprecated("new api", since="1.0")
@nowarn("cat=deprecation")
inline def f =
  g

transparent inline def body =
  g: @nowarn @unchecked
  g: @unchecked @nowarn
