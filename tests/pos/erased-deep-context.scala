object Test {
  def outer1(): Int = {
    def inner erased (a: Int): Int = 0
    inner(42)
  }

  def outer2(): Int = {
    def inner erased (b: Int): Int = {
      def inner2 erased (a: Int): Int = 0
      inner2(b)
    }
    inner(42)
    42
  }

}