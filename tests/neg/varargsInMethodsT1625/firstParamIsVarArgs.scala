trait T1 {
  def foo(x: String*, y: String): Int // error: *-parameter must come last
}