object companions2 {
  transparent def foo() = {
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
    private transparent def getAncestor(p: A): A = {
      p.b.getAncestor(p)
    }
  }
}
