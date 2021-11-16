trait F[x]
implicit def foo[f[_], y, x <: f[y]](implicit ev: F[y]): F[x] = ???
val test = implicitly // error