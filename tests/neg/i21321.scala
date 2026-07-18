
val v1: scala.ContextFunction0[String] = "x" // error
val v1b: scala.ContextFunction0[String] = () ?=> "x" // error
val v2: () ?=> String = "y" // error // error in parser
