//> using options -Xfatal-warnings

object Test:

  println((3: Boolean | Int).isInstanceOf[Boolean])

  println(3.isInstanceOf[Boolean])  // warn

// nopos-error: No warnings can be incurred under -Werror.
