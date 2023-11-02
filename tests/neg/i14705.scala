//> using options -Xfatal-warnings

val n = Nil
val b = n.head.isInstanceOf[String] // warn

// nopos-error: No warnings can be incurred under -Werror.

