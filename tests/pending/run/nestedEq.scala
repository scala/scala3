    class Outer {

      case class Inner()
      case class Inner2()

      val inner: Inner = new Inner
      val inner2 = new Inner2

      def checkInstance(o: Outer) =
        o.inner.isInstanceOf[this.Inner]

      def checkPattern1(i: Any) =
        i match {
          case _: Inner => true
          case _ => false
        }

      def checkPattern2(i: Any) =
        i match {
          case Inner() => true
          case _ => false
        }

      def checkEquals(o: Outer) =
        o.inner == inner

    }

    object Test {

      def main(args: Array[String]) = {
        val o1 = new Outer
        val o2 = new Outer
        println(o1.inner.hashCode)
        println(o2.inner.hashCode)
        println(o2.inner2.hashCode)
        assert(o1.checkInstance(o2))
        assert(!o1.checkPattern1(o2.inner))
        assert(!o1.checkPattern2(o2.inner))
        assert(!o1.checkEquals(o2))
      }

    }
