object TupleExample {
  import Tuple.*

  type A
  type B
  type C

  summon[Concat[A *: B *: EmptyTuple, C *: EmptyTuple]    =:=    A *: B *: C *: EmptyTuple]

  summon[Concat[A *: B *: EmptyTuple, C *: Tuple]   =:=    A *: B *: C *: Tuple]

  summon[Concat[A *: B *: Tuple, C *: EmptyTuple]   <:<    A *: B *: Tuple]
}
