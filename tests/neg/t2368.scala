class C {
  def foo() = {
    var x: String = _ // error: local variables can't be left uninitialized
  }
}
