object Test {
  def foo0(a: Int): Int = a
  def foo1 erased (a: Int): Int = {
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
  erased def foo2(a: Int): Int = {
    foo0(a) // OK
    foo1(a) // OK
    foo2(a) // OK
    foo3(a) // OK
    a // OK
  }
  erased def foo3 erased (a: Int): Int = {
    foo0(a) // OK
    foo1(a) // OK
    foo2(a) // OK
    foo3(a) // OK
    a // OK
  }
}