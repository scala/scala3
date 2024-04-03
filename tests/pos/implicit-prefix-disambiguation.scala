class I[X]

trait A:
  given I[B] = ???
object A extends A

trait B extends A
object B extends B

//import B.given, A.given

def Test = summon[I[B]]


