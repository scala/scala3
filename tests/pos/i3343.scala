object Test {
  def foo[C[_]](implicit t: C[Char] => Traversable[Char]): C[Char] = ???

  val a: List[Char] = foo
}
