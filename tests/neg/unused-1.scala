object Test {
  def foo0(a: Int): Int = a
  def foo1(unused a: Int): Int = {
    foo0(a) // error
    foo0({
      println()
      a // error
    })
    foo1(a) // OK
    a // error
    a // error
  }
}