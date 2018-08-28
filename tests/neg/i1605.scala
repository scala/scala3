object Test {
  def foo = inlineMe

  rewrite def inlineMe = 1 + x2233 // error
}
