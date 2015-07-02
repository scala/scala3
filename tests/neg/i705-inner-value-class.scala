class Foo {
  class B(val a: Int) extends AnyVal // error
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


