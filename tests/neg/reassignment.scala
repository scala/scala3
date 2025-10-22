//> using options -explain

val x = 1
val y = x = 0  // error
val z: Boolean = x = 0 // error
def f = if x = 0 then 1 else 2  // error
def g: Boolean =
  x = 0  // error
  x = 2  // error

