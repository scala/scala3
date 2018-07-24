class StringOps(val s: String) extends AnyVal {
  def lines: Iterator[String] = new collection.AbstractIterator[String] {
    private[this] var index = 0
    def hasNext: Boolean = false
    def next(): String = {
      index = 1
      ""
    }
  }
}

class FooOps(val s: String) extends AnyVal {
  private[this] def bar = 2
  def foo = bar
}
