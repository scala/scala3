trait D {
  val n: Int = 10
}

class M(x: Int) {

  def this(d: D) = {
    this(d.n)

    class L1(x: Int) { val n: Int = 5 }

    class A(b: B, x: Int) {
      println(d.n)

      class L2(x: Int) { val n: Int = 5 }

      def this(b: B) = {
        this(b, 5)
        println(d.n)

        class Inner() {
          println(d.n)
          println(b.n)
          def foo() = println(b.n)
        }
        Inner().foo()

        val l1 = new L1(3)
        println(l1.n)

        val l2 = new L2(3)
        println(l2.n)

        (() => new A(b, 3))()  // ok
      }
    }

    class B(val d: D) {
      val n: Int = 10
    }

    new A(new B(new D))  // error

    trait T {
      val m: Int = 10
    }

    class C(b: B) extends A(b) with T {
      def this(b: B, x: Int) = this(b)
    }

    class D {
      val b = new B(this)
      val c = new C(b, 5)   // error
    }
  }
}

class N(d: D) extends M(d) {
  val msg = "Scala"
}
