import compiletime.ops.int.*

type TupleIndex[T <: Tuple, A, I <: Int] <: (Any, Int) = T match
  case A *: _ => (A, I)
  case _ *: t => TupleIndex[t, A, S[I]]

def tupleIndex[T <: Tuple, A](using i: ValueOf[Tuple.Elem[TupleIndex[T, A, 0], 1]]): Int = i.value
