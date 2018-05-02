class X {
  object A {                         // error
    name.size                        // error
    def foo: Int = name.size         // error
    def bar: Int = 10
  }

  A.foo                              // error
  A.bar                              // ok: forced

  val name = "jack"
}


class Y {
  class A {
    name.size                        // error
    def foo: Int = name.size         // error
    def bar: Int = 10
  }

  (new A).foo                        // error
  (new A).bar                        // error

  val name = "jack"
}