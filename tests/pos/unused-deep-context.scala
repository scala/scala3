object Test {
  unused def outer1(): Int = {
    def inner(unused a: Int): Int =
      a
    inner(42)
  }

  def outer2(): Int = {
    unused def inner(unused b: Int): Int = {
      def inner2(unused a: Int): Int = a
      inner2(b)
    }
    inner(42)
    42
  }

  def outer3(): Int = {
    def inner(unused b: Int): Int = {
      unused def inner2(unused a: Int): Int = a
      inner2(b)
      42
    }
    inner(42)
    42
  }

  unused def outer4(): Int = {
    def inner(unused b: Int): Int = {
      def inner2(unused a: Int): Int = a
      inner2(b)
    }
    inner(42)
    42
  }
}