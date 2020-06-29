type P[x] <: Boolean = x match {
  case 3 => false
  case _ => true
}
type RejectAll[x] = false

def Test =
  summon[Tuple.Filter[(1, 2, 3, 4), P] =:= EmptyTuple] // error
  summon[Tuple.Filter[(1, 2, 3, 4), RejectAll] =:= (1, 2, 3)] // error
  summon[Tuple.Filter[EmptyTuple, P] =:= (1, 2, 3)] // error
