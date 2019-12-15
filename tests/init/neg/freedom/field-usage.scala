class A {
  private[this] val a: Int = 10
  private[this] val b: Int = a * a
}

class B {
  private[this] val b: Int = a * 5     // error
  private[this] val a: Int = 10
}

class C {
  val a: Int = 10
  val b: Int = a * 5        // error: public field is method call
}