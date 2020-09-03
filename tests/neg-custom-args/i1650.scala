object Test {
  test4(test4$default$1)
  def test4[T[P]](x: T[T[List[T[X forSome { type X }]]]]) = ??? // error // error
  def test4$default$1[T[P]]: T[Int] = ???
}
