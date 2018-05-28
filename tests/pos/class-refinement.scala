object Test {

  class C {

    class I

  }

  trait T

  val x: C { type I <: T } = ???  // direct refinement of class member

  val y: x.I = ???

}

class B {
  class C {
    type I
  }
  trait T

  type CC <: C

  val x: CC { type I <: T } = ???
}

object Test2 extends B {

  class CC extends C { class I }

  val y: x.I = ???        // indirect refinement of class member


}

