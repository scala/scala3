class A(b: B) {
  val b2 = new B(this) // error
}

class B(a: A) {
  val a2 = new A(this) // error
}
