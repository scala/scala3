object companions2 {
  inline def foo() = {
    class C {
      println(C.p)
    }

    object C {
      private val p = 1
    }
  }
}

class A {
  val b = new B

  class B {
    private def getAncestor2(p: A): A = p
    private inline def getAncestor(p: A): A = {
      p.b.getAncestor(p)
    }
  }
}
