class B {
  def f: String = "hello"
}

class A(a: Any) extends B {
  def this() = {
    this(f)           // error
  }
}
