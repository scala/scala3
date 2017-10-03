import dotty.unused

object Test {
  def foo1(@unused a: Int): Int = {
    foo1(a) // OK
    a // error
  }
}