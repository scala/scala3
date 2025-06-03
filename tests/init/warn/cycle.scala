class A(x: B) {
  println(x.b)
  val a = new B(this)  // warn
  val d = a.b
}

class B(x: A) {
  println(x.a)
  val b = new A(this)  // warn
  val d = b.a
}