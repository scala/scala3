class A {
  val f: Int = 10

  class B {
    val a = f
  }

  println(new B)              // OK, can promote B early
}

class C extends A {
  override val f: Int = 20    // warn
}