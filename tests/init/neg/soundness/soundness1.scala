class A(_b: B) {
  val b2 = new B(this)
}

class B(_a: A) {
  val a2 = new A(this)
}
