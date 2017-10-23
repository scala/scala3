object Test {
  def foo(): Unit = {
    val elems: Iterator[Int] = ???
    do {
      elems.next()
      do elems.next() while (elems.hasNext)
    } while (elems.hasNext)
  }
}
