object t1 {
  rewrite def construct[Elem, Coll[_]](xs: List[Elem]): Coll[Elem] = ???

  val xs3 = construct[Coll = List](List(1, 2, 3))
}
