object Test {
  def outer1(): Int = {
    def inner(unused a: Int): Int = 0
    inner(42)
  }

  def outer2(): Int = {
    def inner(unused b: Int): Int = {
      def inner2(unused a: Int): Int = 0
      inner2(b)
    }
    inner(42)
    42
  }

}