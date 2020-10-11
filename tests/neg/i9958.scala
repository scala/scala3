val x = summon[[X] =>> (X, X)] // error
// #9971:
trait G[F[_]]

def f(x: G[List]) = ???

def a: G[[A <: Int] =>> List[A]] = ???
def b = f(a) // error

