class Position(val foo: Int) extends AnyVal {
  def orElse(that: Position) =
    if (foo != 0) this else that
}
