object Test {
  class A(x: Int) {
    macros.test(x)
  }
  class B(y: String) {
    macros.test(y) // error
  }
}