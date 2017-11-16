object Test {
  def foo0(a: Int): Int = a
  def foo1(unused a: Int): Int = {
    foo0(
      a // error
    )
    foo0({
      println()
      a // error
    })
    foo1(a) // OK
    foo2( // error
      a // error
    )
    foo3( // error
      a
    )
    a // error
  }
  unused def foo2(a: Int): Int = {
    foo0(a) // OK
    foo1(a) // OK
    foo2(a) // OK
    foo3(a) // OK
    a // OK
  }
  unused def foo3(unused a: Int): Int = {
    foo0(a) // OK
    foo1(a) // OK
    foo2(a) // OK
    foo3(a) // OK
    a // OK
  }
}