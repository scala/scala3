object unions {

  class A {
    def f: String = "abc"
  }

  class B {
    def f: String = "bcd"
  }

  val x: A | B = if (true) new A else new B
  println(x.f)

}
