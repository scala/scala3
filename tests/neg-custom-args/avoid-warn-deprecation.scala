object A {
  @deprecated("use bar instead of this one", "0.2.3")
  def foo: Int = 3
}

object B {
  A.foo
}
// nopos-error there was 1 deprecation warning; re-run with -deprecation for details
