class Test {
  def isTraversableAgain(from: Iterator[Int]): Boolean =
    from.isInstanceOf[Iterable[?]]
}
