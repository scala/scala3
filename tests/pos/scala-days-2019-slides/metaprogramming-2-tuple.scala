object TupleExample {
  import Tuple._

  type A
  type B
  type C

  summon[Concat[A *: B *: Unit, C *: Unit]    =:=    A *: B *: C *: Unit]

  summon[Concat[A *: B *: Unit, C *: Tuple]   =:=    A *: B *: C *: Tuple]

  summon[Concat[A *: B *: Tuple, C *: Unit]   <:<    A *: B *: Tuple]
}
