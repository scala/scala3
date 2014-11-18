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
    trait Ax extends super.Ax { def xxx : Int; }
    abstract class InnerB extends InnerA {
      // type B <: A;
      val a : A = doB;
      a.xxx;
      doB.xxx;
    }
  }

package p1 {
  abstract class T1 {
    protected def bug(p: Int = 1): Int // without 'protected' compiles fine
  }
}
package p2 { // all being in the same package compiles fine
  import p1._
  abstract class T2 extends T1 {
    class A {
      bug()
    }
  }

  abstract class T3 extends T2 {
    class A {
      bug()
    }
  }
}

class A[T] {

  def f(x: T)(y: T = x) = y

}

class B extends A[Int] {

  def f(x: Int)(y: Int) = y

  f(2)()

}

class X {
  def f: A[Int] = ???
}
class Y extends X {
  def f: A[Int] = ???
}


class A1
class B1

class X1 {
  def f(): A1 = ???
}
class Y1 extends X1 {
  override def f(): B1 = ???
}
