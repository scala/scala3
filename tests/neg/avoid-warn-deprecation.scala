//> using options -Xfatal-warnings -feature

object A {
  @deprecated("use bar instead of this one", "0.2.3")
  def foo: Int = 3
}

object B {
  A.foo
}
// nopos-warn there was 1 deprecation warning; re-run with -deprecation for details
// nopos-error: No warnings can be incurred under -Werror.