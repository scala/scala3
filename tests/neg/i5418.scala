class Test {
  class Tree[A]

  def fromOrderedKeys[A](xs: Iterator[A]): Tree[A] = ???

  def from[E](it: Iterable[E]): Tree[E] =
    it match {
      case r: Range =>
        val it = r.iterator

        // instantiation of covariant GADTs is unsound
        fromOrderedKeys(it) // error: type mismatch: found: Iterator[Int](it), required Iterator[E]
    }
}
