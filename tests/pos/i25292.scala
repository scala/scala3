//> using options -Ycheck:all

val namedTuple: (x: 42, y: true) = (x = 42, y = true)
val a = namedTuple match
  case (1, y) => ()
  case (x, y) => ()

val tuple: (42, true) = (42, true)
val b = tuple match
  case (1, y) => ()
  case (x, y) => ()
