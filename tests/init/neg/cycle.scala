class A(x: B) {
  println(x.b) // error
  val a = new B(this)
  val d = a.b
}

class B(x: A) {
  println(x.a) // error
  val b = new A(this)
  val d = b.a
}