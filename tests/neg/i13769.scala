val tup = (1, "s")
val te = tup.map((x: _ <: Int) => List(x)) // error // error
