class A(x: B) {
  println(x.b)
  val a = new B(this)  // error
  val d = a.b
}

class B(x: A) {
  println(x.a)
  val b = new A(this)  // error
  val d = b.a
}