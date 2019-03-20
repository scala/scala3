type K[T <: AnyKind] = T
val a: K[Int] = 1
val b: K[List][Int] = Nil