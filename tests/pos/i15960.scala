class Foo(open: String) {
  def bar(n: Int) = n match {
    case 0 => open
    case _ => throw new IndexOutOfBoundsException()
  }
  def baz() = open
}


