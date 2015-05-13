object O {
  def foo[T](t: T) = 0
  def foo(s: String)(implicit i: DummyImplicit = null) = 1
}

object Test extends dotty.runtime.LegacyApp {
  assert(O.foo("") == 1)
}
