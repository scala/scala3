object Test {
  def foo(x: Int): Int = x

  Some(foo): Option[Int => Int]
    // missing arguments for method foo
    // follow this method with `_' if you want to treat it as a partially applied function
}
