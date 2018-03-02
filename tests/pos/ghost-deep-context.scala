object Test {
  def outer1(): Int = {
    def inner(ghost a: Int): Int = 0
    inner(42)
  }

  def outer2(): Int = {
    def inner(ghost b: Int): Int = {
      def inner2(ghost a: Int): Int = 0
      inner2(b)
    }
    inner(42)
    42
  }

}