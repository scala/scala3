class Position(val foo: Int) extends AnyVal {
  def orElse(that: Position) =
    if (this != null) this else that
}
