trait T {
  def valid: Boolean
  def f(v: {v: Unit with valid}): Boolean
}

case class CT() extends T {
  final def valid: Boolean = true
  def f(v: {v: Unit with this.valid}): Boolean = true
}
