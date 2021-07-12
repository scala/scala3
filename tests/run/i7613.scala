trait Foo[A]
trait Bar[A] extends Foo[A]
trait Baz[A] extends Bar[A]

trait FooLaws[A](using Foo[A])
trait BarLaws[A](using Bar[A]) extends FooLaws[A]
trait BazLaws[A](using Baz[A]) extends BarLaws[A]

def instance1[A](using Baz[A]): BazLaws[A] =
  new FooLaws[A] with BarLaws[A] with BazLaws[A] {}

def instance2[A](using Baz[A]): BazLaws[A] =
  new BazLaws[A] {}

trait I:
  def show(x: String): Unit
class A
trait B1(using I) extends A { summon[I].show("B1") }
trait B2(using I) extends B1 { summon[I].show("B2") }
trait C1 extends B1
trait C2 extends B2
class D(using I) extends A, C1
class E(using I) extends D(using new I { def show(x: String) = println(s"superD: $x")}), C2
class F(using I) extends A, C2

@main def Test =
  D(using new I { def show(x: String) = println(s"D: $x")})
  E(using new I { def show(x: String) = println(s"E: $x")})
  F(using new I { def show(x: String) = println(s"F: $x")})
