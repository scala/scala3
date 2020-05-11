import scala.quoted._

inline def foo[T](using s: Scope)(using s.Type[T]): Int = 10

def main(using s: Scope) = {
  type S = Int
  foo[S]
  foo[Int]

  type T = Int => Int
  foo[T]
  foo[Int => Int]

  type U = List[Int]
  foo[U]
  foo[List[Int]]

  type N[+X] = List[X]
  foo[N]
  foo[List]

  type V = List[S]
  foo[V]

  type B = V => T
  foo[B]
}
