extension(a: Int)
  def b: Int = ???
  def h: Unit =
    new Function1[Int, Int] {
      def apply(r: Int): Int = b
    }
