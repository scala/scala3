class A(a: Any) {
  def this() = {
    this(b)           // error
  }

  def b = new {}
}
