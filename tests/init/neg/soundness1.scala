class A(b: B) {
  val b2 = new B(this)
}

class B(a: A) {
  val a2 = new A(this)
}

object Test2:
  class A(b: B) {
    val b2 = new B(this)
    val c = b2.a2
  }

  class B(a: A) {
    val a2 = new A(this)
    val c = a2.b2
  }

object Test3:
  class A(b: B) {
    println(b.a2)
    val b2 = new B(this)    // error
  }

  class B(a: A) {
    println(a.b2)
    val a2 = new A(this)    // error
  }
