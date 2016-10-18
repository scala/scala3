object Test {
  def foo = inlineMe

  inline def inlineMe = 1 + x // error
}
