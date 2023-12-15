object A { val x: String = B.y }  
object B { val y: String = A.x }  

// nopos-error: No warnings can be incurred under -Werror.