//> using options -Werror

class Test:
  inline def count(inline x: Boolean) = x match
    case true => 1
    case false => 0

  assert(count(true) == 1)
  assert(count(false) == 0)
  var x = true
  assert(count(x) == 1)
