object Test {
  @annotation.tailrec
  def foo(i: given Unit => Int): given Unit => Int =
    if (i == 0)
      0
    else
      foo(i - 1)*2 // error: Cannot rewrite recursive call not in tail position
}
