trait T3 {
  def foo(x: String*, y: String*, c: String*): Int // error // error: varargs parameter must come last
}