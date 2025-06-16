import Tuple.*

def test =
  summon[Disjoint[(1, 2, 3), (4, 5)] =:= true]
  summon[Disjoint[(1, 2, 6), (4, 5)] =:= true]
  summon[Disjoint[(1, 2, 6), EmptyTuple] =:= true]
  summon[Disjoint[EmptyTuple, EmptyTuple] =:= true]

  summon[Contains[(1, 2, 3), Int] =:= true]
  summon[Contains[(1, 2, 3), 2] =:= true]
  summon[Contains[(1, 2, 3), 4] =:= false]

  summon[Disjoint[(1, 2, 3), (4, 2)] =:= false]
  summon[Disjoint[("a", "b"), ("b", "c")] =:= false]
  summon[Disjoint[(1, 2, 6), Tuple1[2]] =:= false]
  summon[Disjoint[Tuple1[3], (4, 3, 6)] =:= false]

