type F[X <: String] = X

val a = summon[F[Int] =:= Int] // error
