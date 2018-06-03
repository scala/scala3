package P {
  class C {
    protected def f(): Int = 22
  }
}

package Q {
  class D extends P.C {
    class Inner {
      transparent def g() = f()
    }
  }
}

object Test extends App {
  import Q._

  val d = new D
  val i = new d.Inner
  val x = i.g()
  assert(x == 22)
}
