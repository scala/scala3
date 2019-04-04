
// Test that we correctly handle nullable unions when widening
// (we don't widen them).
class Test {
  def foo(): Unit = {
    val x: String|Null = ???
    val y = x // this used to crash the compiler
  }
}
