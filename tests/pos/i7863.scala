def f[F[_]] = ()

inline def g = f[[R] =>> Int => R]

val a = g