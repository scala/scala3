package p {
  package a {

    /** Test type parameters */
    abstract class PolyA[a] {
      protected def m(x: a): Unit;

      class B {

        trait Node {
          def s: String = "";
        }
        protected def tie(x: Node): Unit = { x.s; () }
      }
    }
  }

  package b {
    import a._;

    abstract class X[T] extends PolyA[T] {

      trait Inner extends B {
        def self: T;
        def self2: Node;
        def getB: Inner;

        m(self)

        trait InnerInner {
          val g = getB
          g.tie(self2.asInstanceOf[g.Node]) // error: acess not permitted
        }
      }
    }
  }
}
