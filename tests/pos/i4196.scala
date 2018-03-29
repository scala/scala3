object Test {
  @annotation.tailrec
  def foo(i: implicit Unit => Int): implicit Unit => Int =
    if (i == 0)
      0
    else
      foo(i - 1)
}
