trait T1 {
  def foo(x: String*, y: String): Int // error: varargs parameter must come last
}