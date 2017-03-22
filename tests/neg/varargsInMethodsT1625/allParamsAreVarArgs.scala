trait T3 {
  def foo(x: String*, y: String*, c: String*): Int // error // error: *-parameter must come last
}