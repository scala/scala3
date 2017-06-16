trait A {
 def foo = 1
}

trait B {
 def foo = 2
}

class C extends A with B {
 override def foo = super[A].foo + super[B].foo
}

class Base[A](exp: => Option[A])

object Empty extends Base[Nothing](None)


trait B1 extends C1 { override def f() = { super.f(); print("B1") }}
trait B2 extends B1 { override def f() = { super.f(); print("B2") }}
trait A1 extends C1 { override def f() = { super.f(); print("A1") }}
trait A2 extends A1 { override def f() = { super.f(); print("A2") }}
class C1 { def f() = print("C1") }
class C2 extends A2 with B2 { override def f() = { super.f(); print("C2") }}


trait B3 extends C1 { override def f() = { super.f(); print("B3") }}
trait B4 extends C1 { this: B3 =>  override def f() = { super.f(); print("B4") }}
class C3 extends C1 with B3 with B4 { override def f() = { super.f(); print("C3") }}

trait DT {
  def f(): Unit
}
trait IT extends DT {
  def f() = { println("IT") }
}
abstract class MPT {
}
trait AT extends MPT with DT {
  abstract override def f() = { super.f(); println("AT") }
}
class ER extends MPT with IT with AT {
  override def f() = { super.f(); println("ER") }
}

object Test {
  def main(args: Array[String]): Unit = {
    assert(new C().foo == 3)
    new C2().f()
    println()
    new C3().f()
    println()
    new ER().f()
    Empty
  }
}
