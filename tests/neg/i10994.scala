//> using options -Xfatal-warnings

def foo = true match
  case (b: Boolean): Boolean => ()  // error
