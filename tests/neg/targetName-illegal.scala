import annotation.targetName

val s = "x"
@targetName(s) def y = 1 // error
