object Test extends App {
  def foo(t: AnyVal): Unit = t match {
    case _: Int =>
  }
  foo(3)
}