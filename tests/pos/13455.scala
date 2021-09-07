sealed class R

type X[T] = T match {
  case R        => String
  case (z => r) => Int
}
def x[T]: X[T] = ???

def i(i0: Int): Unit = ???
val a = i(x[Int => String])
