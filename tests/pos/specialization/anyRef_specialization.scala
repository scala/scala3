object Test {
  def foo[@specialized(AnyRef) T](t: T): T = t
}
