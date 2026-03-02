object A { val x: String = B.y } // warn
object B { val y: String = A.x } // warn
