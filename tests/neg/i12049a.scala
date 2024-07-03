trait A
trait B
type M[X] = X match
  case A => Int
  case B => String
val x: String = ??? : M[B] // error

type Last[X <: Tuple] = X match
  case _ *: _ *: t => Last[t]
  case t *: EmptyTuple => t

val y1: Int = ??? : Last[Int *: EmptyTuple]
val y2: String = ??? : Last[Int *: Boolean *: String *: EmptyTuple]
val y3: String = ??? : Last[Int *: Int *: Boolean *: String *: EmptyTuple]  // error

type Reverse[X <: Tuple] = X match
  case t1 *: t2 *: ts => Tuple.Concat[Reverse[ts], (t2, t1)]
  case EmptyTuple => EmptyTuple

val z1: (B, A) = ??? : Reverse[(A, B)]
val z2: (B, A, B, A) = ??? : Reverse[(A, B, A, B)]
val z3: (A, B, A) = ??? : Reverse[(A, B, A)] // error

val _ = summon[M[B]]  // error
val _ = summon[String =:= Last[Int *: Int *: Boolean *: String *: EmptyTuple]] // error
val _ = summon[(A, B, A) =:= Reverse[(A, B, A)]] // error

val _ = (??? : M[B]).length // error

