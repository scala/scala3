class SecondaryCtor(val x: Int) extends AnyVal {
  def this(a: Int, b: Int) = { // error
    this(a + b)
  }
}
