class C(b: B)

object O extends C(B()) {   // error
  val a: Int = new B().n

  object A {
    val b: Int = 5
  }

  val c: Int = A.b + 4
}

class B {
  val n: Int = O.A.b
}