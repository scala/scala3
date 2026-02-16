type Kinded[F[_]] = F[Any] | F[Nothing]

def values[F[_]]: Vector[Kinded[F]] = ???

def mapValues[F[_], T](f: Kinded[F] => T): Vector[T] = values[F].map { case x => f(x) }
