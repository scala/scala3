object Test {
  def foo = inlineMe

  transparent def inlineMe = 1 + x2233 // error
}
