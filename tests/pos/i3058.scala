trait C[T] { type U }

object C { type Aux[X, Y] = C[X] { type U = Y } }


def a: C[Int] { type U = Int } = ???

def b[T](c: C[T]): C[T] { type U = c.U } = ???


def t[T, U](c: C.Aux[T, U]) = ???


object Test { t(b(a)) }
