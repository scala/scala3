// https://github.com/scala/scala3/issues/7445

type O1[A] = {
  type OutInner[Ts] <: Tuple = Ts match {
    case EmptyTuple => EmptyTuple
    case h *: t => h *: OutInner[t]
  }

  type Out = OutInner[A]
}

def f1: O1[(Int, Int)]#Out = ???
