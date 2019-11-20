type F[T]
type G[T]
type Stuff
given Stuff = ???

def (x: T) f[T](given Stuff): F[T] = ???


def g1[T](x: T): F[G[T]] = x.f(given summon[Stuff]) // error

def g2[T](x: T): F[G[T]] = x.f // error

def g3[T](x: T): F[G[T]] = f(x)(given summon[Stuff]) // error
