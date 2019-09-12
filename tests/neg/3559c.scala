class A(a: Any) {
  def this() = {
    this(a)           // error
  }

  def b = new {}
}
