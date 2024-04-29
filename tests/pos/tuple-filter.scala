type P[x] <: Boolean = x match {
  case 3 => false
  case _ => true
}
type RejectAll[x] = false

def Test =
  summon[Tuple.Filter[(1, 2, 3, 4), P] =:= (1, 2, 4)]
  summon[Tuple.Filter[(1, 2, 3, 4), RejectAll] =:= EmptyTuple]
  summon[Tuple.Filter[EmptyTuple, P] =:= EmptyTuple]

  import compiletime.ops.int.<
  summon[Tuple.Filter[(1, 4, 7, 2, 10, 3, 4), [X <: Int] =>> X < 5] =:= (1, 4, 2, 3, 4)]
