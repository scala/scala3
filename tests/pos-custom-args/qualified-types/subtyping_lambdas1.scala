
def test: Unit =
  summon[{l: List[Int] with true} =:= {l: List[Int] with true}]
  summon[{l: List[Int] with l == Nil} =:= {l: List[Int] with l == Nil}]
  summon[{l: List[Int] with l.forall(x => x > 0)} =:= {l: List[Int] with l.forall(y => y > 0)}]
