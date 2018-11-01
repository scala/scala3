class A {
  val x = "hello"

  class Inner1 {
    def f(n: Int) = println(new Inner1)
    val y = (n: Int) => f(20)
  }

  class Inner2 {
    val y = x
  }
}

class B extends A {
  println(new Inner1)
  println(new Inner2)  // error

  override val x = "world"
}