// `termAssumptions` must not equate a mutable binding with its initializer:
// the variable may have been reassigned since.
def t =
  var x = 3
  x = 4
  val y: Int with y == 3 = x // error
