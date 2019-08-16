import given scala.TupleOps

val x: (1, 2, 3) = (1, 2, 3)
val y: (4, 5, 6) = (4, 5, 6)

val z: ((1, 4), (2, 5), (3, 6)) = x.zip(y)
