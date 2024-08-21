
class I[X]
class J[X]

trait A:
  given I[B] = ???
  given (using I[B]): J[B] = ???
object A extends A

trait B extends A
object B extends B

//import B.given, A.given

def Test =
  summon[I[B]]
  summon[J[B]]
