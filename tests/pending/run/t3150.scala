    object Test {
      case object Bob { override def equals(other: Any) = true }

      class Bob2 {
        override def equals(other: Any) = true
      }
      val Bob2 = new Bob2

      def f0(x: Any) = x match { case Bob2 => Bob2 } // class cast exception at runtime, dotc only
      def f1(x: Any) = x match { case Bob => Bob } // class cast exception at runtime, dotc only
      def f2(x: Any): Bob.type = x match { case x @ Bob => x } // class cast exception at runtime, dotc and javac.

      def main(args: Array[String]): Unit = {
        assert(f0(Bob2) eq Bob2)
        assert(f0(0) eq Bob2)  // only dotty fails here
        assert(f0(Nil) eq Bob2)

        assert(f1(Bob) eq Bob)
        assert(f1(0) eq Bob)  // only dotty fails here
        assert(f1(Nil) eq Bob)

        assert(f2(Bob) eq Bob)
        assert(f2(0) eq Bob) // both dotty and scalac fail here
        assert(f2(Nil) eq Bob)
      }
    }
