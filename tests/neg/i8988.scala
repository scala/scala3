val xs = List(1, 2, 3)
val ys = xs.tail
val zs: String = ys // error: (but type should not mention @uncheckedVariance)
