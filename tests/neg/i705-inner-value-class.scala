class Foo {
  class B(val a: Int) extends AnyVal // error
}

class VCwithBadMembers(val a: Int) extends AnyVal {
  def this() = this(1) // error
  var x = 0 // error
  val y = 2 // error
  println("hi") // error
}

object Test {
  class B(val a: Int) extends AnyVal // ok
  def f = {
    class C(val a: Int) extends AnyVal // error
    new C(1)
  }
  class B1(val b: Int) extends B(b)
//  class D extends B( { class E(val a: Int) extends AnyVal; new E(1) } ) // error
}


