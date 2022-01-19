object Test {
  def +[T](x: T): String = "x"
  +[Int](6): String // Parser can treat + as identifier in non-unary context
  +(6): Int // Parser prioritizes + as unary when possible
}
