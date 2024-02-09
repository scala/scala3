def byName[T](p: => T): T = p
val test = (if ??? then byName else (??? : ((=> Int) => Int))) (42)