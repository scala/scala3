object Test {
  def foo0(a: Int): Int = a
  def foo1(ghost a: Int): Int = {
    foo0(
      u // error
    )
    foo1(u) // OK
    foo2( // error
      u // error
    )
    foo3( // error
      u
    )
    u // error
    u // error
  }
  ghost def foo2(a: Int): Int = {
    foo0(u) // OK
    foo1(u) // OK
    foo2(u) // OK
    foo3(u) // OK
    u // warn
    u // OK
  }
  ghost def foo3(ghost a: Int): Int = {
    foo0(u) // OK
    foo1(u) // OK
    foo2(u) // OK
    foo3(u) // OK
    u // warn
    u // OK
  }

  ghost val foo4: Int = {
    foo0(u) // OK
    foo1(u) // OK
    foo2(u) // OK
    foo3(u) // OK
    u // warn
    u // OK
  }

  ghost def u: Int = 42
}