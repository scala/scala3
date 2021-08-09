// scalac: -Werror -deprecation -feature
def foo = true match
  case (b: Boolean): Boolean => ()
