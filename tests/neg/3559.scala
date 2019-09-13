class A(a: Any) {
  def this() = {
    this(b)           // error: forward reference not allowed from self constructor invocation
    def b = new {}
  }
}
