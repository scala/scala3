class IntDeco(x: Int) extends AnyVal {
  def foo(ghost y: Int) = x
}
