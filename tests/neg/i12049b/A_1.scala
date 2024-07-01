
type Last[X <: Tuple] = X match
  case _ *: _ *: t => Last[t]
  case t *: EmptyTuple => t

def foo: Last[Int *: Int *: Boolean *: String *: EmptyTuple] = ???
