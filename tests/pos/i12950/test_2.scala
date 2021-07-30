import repro.repro.{*, given}

val x = 1.lift[Two]
val _ = x.repro[2]
val y = 1.lift[2]
val _ = y.repro[2]
