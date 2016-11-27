import scala.annotation.internal.DoNotDCE

object DCETest {
  @DoNotDCE def dceTest: Unit = {
    System.out.println("dceTest")

    val a = new A
    Test.shouldDCE(a.f1)
    a.hashCode()
    a.equals(a)
    a.toString

    val b = new B {}
    Test.shouldDCE(b.f1)
    b.hashCode()
    b.equals(b)
    b.toString

    val c = new C {}
    Test.shouldDCE(c.f1)
    c.hashCode()
    c.equals(c)
    c.toString
  }

  @scala.export def entryPoint(): Unit = {
    System.out.println(new A)
    System.out.println(new B {})
    System.out.println(new C {})
  }
}

class A {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
}

abstract class B {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
}

trait C {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
}
