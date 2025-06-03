class Test { test =>
  class A {
    val self = this
  }

  val a = new A
  println(a)


  class B {
    val self = this
    val outer = test
  }

  val b = new B
  println(b)      // warn

  val n = 10
}