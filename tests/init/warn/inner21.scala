class X {
  object A {
    name.size
    def foo: Int = name.size
    def bar: Int = 10
  }

  A.foo
  A.bar

  val name = "jack"                  // warn
}


class Y {
  class A {
    name.size
    def foo: Int = name.size
    def bar: Int = 10
  }

  (new A).foo
  (new A).bar

  val name = "jack"                   // warn
}