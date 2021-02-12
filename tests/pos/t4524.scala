object test {
  import A.*
  class A(b: B = new A.B())
  object A {
    class B
    new A()
  }
}

