class I
val c: (i: I) ?=> i.type = x ?=> x
val d: (i: I) ?=> i.type = c
val i = c  // error
