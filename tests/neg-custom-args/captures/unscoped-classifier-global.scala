import caps.*

trait Async extends Control

class A(a: Async) extends caps.Unscoped // error but msg could be better

class B extends caps.Unscoped:
  val a: Async^ = new Async {}  // ok (should this be an error instead?)

class C(f: () => Unit) extends caps.Unscoped // error but msg could be better

class D extends caps.Unscoped:
  val f: () => Unit = ???   // ok (should this be an error instead?)

val g: () => Unit = () => ()

def test() =
  class E extends caps.Unscoped:
    def gg() = g()     // error but msg could be better

  val b = B()
  val d = D() // ok (?)
  val _: D^{any.only[Unscoped]} = d
