class A {
  val f: Int = 10

  class B {
    val a = f
  }

  println(new B)              // error: warm value
}

class C extends A {
  override val f: Int = 20    // warn
}