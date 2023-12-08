//> using options -Werror
// minimisation of a regression that occurred in bootstrapping
class Test:
  def t(a: Boolean, b: Boolean) = (a, b) match
    case (false, false) => 1
    case (false, true ) => 2
    case (true,  false) => 3
    case (true,  true ) => 4
