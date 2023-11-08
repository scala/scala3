object A { val x: String = B.y }  // error
object B { val y: String = A.x }  // error

// nopos-error: No warnings can be incurred under -Werror.