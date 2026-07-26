//> using options -Werror

def unreachableEmptyTupleCase(tuple: Tuple): Unit = tuple match
  case Tuple() => ()
  case EmptyTuple => () // error
  case _ => ()
