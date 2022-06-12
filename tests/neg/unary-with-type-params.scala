object Test {
  def +[T](x: T): String = "x"
  +[Int](6): String // error: expression expected but '[' found
}
