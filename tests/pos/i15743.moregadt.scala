enum SUB[-A, +B]:
  case Refl[C]() extends SUB[C, C]
import SUB._

type <:<[A, B] = SUB[A, B]

def foo[A, T](ev: T <:< Tuple) = ev.match { case Refl() =>
  val t1: Int *: T = ???
  val t2: Int = t1.head  // works

  val t3: A *: T = ???
  val t4: A = t3.head    // boom

  val t5: Tuple.Head[Int *: T] = 0  // boom
  val t6: Tuple.Head[A *: T] = t4  // boom
}
