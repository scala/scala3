//> using options -Werror
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)

def unreachableEmptyTupleCase(tuple: Tuple): Unit = tuple match
  case Tuple() => ()
  case EmptyTuple => ()
  case _ => ()
