object Test {
  @annotation.tailrec
  def foo(i: implicit Unit => Int): implicit Unit => Int =  // error: method not tail recursive
    if (i == 0)
      0
    else
      foo(i - 1)*2 // error: Cannot rewrite recursive call not in tail position
}
