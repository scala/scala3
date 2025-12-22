object TraitExtendingClass {

  /*abstract*/
  class A()(
      implicit
      val x: Int = 3
  )

  trait B extends A

  class C(implicit c: Int = 42)

  @main def main(): Unit = {

    val a = new A()
    val b = new B {} // error happens here
    val c = new C()

    println(b.x)
  }
}
def f(i: Int)(j: Int)(implicit x: Int = 42) = x*(i+j)
def g = f(1)(1)

abstract class A()(implicit val x: Int = 3)
class A1 extends A
class A2 extends A()

abstract class B()(using y: Int = 9)
class B1 extends B
class B2 extends B()

abstract class C(implicit val x: Int = 3)
class C1 extends C
class C2 extends C()

abstract class D(using y: Int = 9)
class D1 extends D
class D2 extends D()

abstract class E()(implicit val x: Int = 3, val y: Int = 4)
class E1 extends E
class E2 extends E()

abstract class F()(using y: Int = 9, x: Int = 8)
class F1 extends F
class F2 extends F()

abstract class G(implicit val x: Int = 3, val y: Int = 4)
class G1 extends G
class G2 extends G()

abstract class H(using y: Int = 9, x: Int = 8)
class H1 extends H
class H2 extends H()
