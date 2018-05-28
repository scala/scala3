object Test {
  def foo(s: Boolean, j: java.lang.Boolean) = {
    val a = s == j
    val b = j == s
  }
}
