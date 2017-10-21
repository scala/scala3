object Test {
  def foo(): Unit = {
    val elems: Iterator[Int] = ???
    while (elems.hasNext) {
      elems.next()
      while (elems.hasNext) elems.next()
    }
  }
}
