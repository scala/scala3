// https://github.com/scala/scala3/issues/14642
case object A
case class B()
case class C()
type Union = A.type | B | C
val a: List[A.type] = ???
val b: List[B] = ???
val c: List[C] = ???
val l1: List[Union] = a ++ b
val l2: List[Union] =
  a ++ b ++ c
val l3: List[Union] =
  (a: List[
    Union
  ]) ++ b ++ c
val l4: List[Union] = (a: List[Union]) ++ (b ++ c)
