import scala.quoted._

inline def foo[T:Type]: Int = 10

def main = {
  type S = Int
  foo[S]
  foo[Int]

  type T = Int => Int
  foo[T]
  foo[Int => Int]

  type U = List[Int]
  foo[U]
  foo[List[Int]]

  type N = List
  foo[N]
  foo[List]

  type V = List[S]
  foo[V]

  type B = V => T
  foo[B]
}
