  abstract class FooA {
    type A <: Ax;
    abstract class Ax;
    abstract class InnerA {
      type B <: A;
      def doB : B;
    }
  }
  trait FooB extends FooA {
    type A <: Ax;
    trait Ax extends super.Ax { def xxx : Int; } // error: cyclic inheritance: trait Ax extends itself
                                                 // (Note that inheriting a class of the same name is no longer allowed)
    abstract class InnerB extends InnerA {
      // type B <: A;
      val a : A = doB;
      a.xxx;
      doB.xxx;
    }
  }

