class Position(val foo: Int) extends AnyVal {
  def thing(): Position = {
    val y = new Position(1)
    y
  }
}
