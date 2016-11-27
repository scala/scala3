import java.util.Observable

import scala.annotation.internal.DoNotDCE

object DCETest {
  @DoNotDCE def dceTest: Unit = {
    System.out.println("dceTest")

    val a = new A
    Test.shouldDCE(a.f1)
    a.hashCode()
    a.equals(a)
    a.toString
    a.update(new Observable, null)

    val b = new B {}
    Test.shouldDCE(b.f1)
    b.hashCode()
    b.equals(b)
    b.toString
    b.update(new Observable, null)

    val c = new C {}
    Test.shouldDCE(c.f1)
    c.hashCode()
    c.equals(c)
    c.toString
    c.update(new Observable, null)
  }

  @scala.export def entryPoint(): Unit = {
    System.out.print(new A)
    System.out.print(new B {})
    System.out.print(new C {})
  }
}

class A extends java.util.Observer {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
  override def update(o: Observable, arg: scala.Any): Unit = ()
}

abstract class B extends java.util.Observer {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
  override def update(o: Observable, arg: scala.Any): Unit = ()
}

trait C extends java.util.Observer  {
  def f1 = 42
  override def hashCode(): Int = super.hashCode()
  override def equals(obj: scala.Any): Boolean = super.equals(obj)
  override def toString: String = super.toString
  override def update(o: Observable, arg: scala.Any): Unit = ()
}
