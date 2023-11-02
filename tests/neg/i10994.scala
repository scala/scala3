//> using options -Xfatal-warnings

def foo = true match
  case (b: Boolean): Boolean => ()  // warn

// nopos-error: No warnings can be incurred under -Werror.
