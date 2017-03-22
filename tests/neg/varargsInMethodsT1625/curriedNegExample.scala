trait T2 {
  def foo(x: String*, y: String*)(z: Boolean*)(u: Int*, l: Int*): Int // error // error: *-parameter must come last
}