object companions2 {
  rewrite def foo() = {
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
    private rewrite def getAncestor(p: A): A = {
      p.b.getAncestor(p)
    }
  }
}
