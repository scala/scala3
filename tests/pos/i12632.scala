class CCC[S](val i: Int) {
  def this() =
    this(
      {
        val z = new Ordering[S] {
          override def compare(x: S, y: S): Int = ???
        }
        3
      })
}
