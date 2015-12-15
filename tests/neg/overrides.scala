class Foo {
  type A = Int
  type B >: Int <: Int
  def get: A = 42
}
class Bar extends Foo {
  override type A = Any   // error
  type B >: String <: Any  // error
  override def get: A = "bar"
}
object Test {
  def main(args: Array[String]): Unit = {
    val foo: Foo = new Bar
    val i: Int = foo.get
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
    class A {   // error: classes cannot be overridden
      bug()
    }
  }
}

class A[T] {

  def f(x: T)(y: T = x) = y

}

class B extends A[Int] {

  def f(x: Int)(y: Int) = y // error: needs `override' modifier

  f(2)()

}

class X {
  def f: A[Int] = ???
}
class Y extends X {
  def f: A[Int] = ??? // error: needs `override' modifier
}


class A1
class B1

class X1 {
  def f(): A1 = ???
}
class Y1 extends X1 {
  override def f(): B1 = ??? // error: has incompatible type
}

class X2 {
  type T = A1
}
class Y2 extends X2 {
  type T = B1 // error: needs `override' modifier
}

class X3 {
  override type T = A1 // error: overrides nothing
}

package p3 {

// Dotty change of rules: Toverrider#f does not
// override TCommon#f, hence the accidental override rule
// applies.
trait TCommon {
  def f: String
}

class C1 extends TCommon {
  def f = "in C1"
}

trait TOverrider { this: TCommon =>
  override def f = "in TOverrider"   // The overridden self-type member...
}

class C2 extends C1 with TOverrider  // ... fails to override, here. // error: accidental override

}

package p4 {

  abstract class C[T] { def head: T }
  case class D[T](head: Int) extends C[T] // error: has incompatible type

}

package p5 {
class A {
  def m: String = "foo"
}

class B extends A {
  override val m: Int = 42 // error: has incompatible type
}

class C extends A {
  override def m: Int = 42 // error: has incompatible type
}
}

