object Test {
  def foo[A <: X, B <: X, X](left: A, right: B): Unit = {
    val elem = if (false) left else right
    val check: X = elem
  }
}
