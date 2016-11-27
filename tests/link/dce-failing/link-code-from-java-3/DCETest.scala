import java.util.Observable

import scala.annotation.internal.DoNotDCE

object DCETest {
  @DoNotDCE def dceTest: Unit = {
    System.out.println("dceTest")

    val a = new A[AA]
    a.hasNext
    a.next()
    val aa = new AA
    Test.shouldDCE(aa.f)
    aa.toString

    val b = new B[BB] {}
    b.hasNext
    b.next()
    val bb = new BB
    Test.shouldDCE(bb.f)
    bb.toString

    val c = new C[CC] {}
    c.hasNext
    c.next()
    val cc = new CC
    Test.shouldDCE(cc.f)
    cc.toString
  }

  @scala.export def entryPoint(): Unit = {
    System.out.print(new A[AA])
    System.out.print(new B[BB] {})
    System.out.print(new C[CC] {})
  }
}

class AA {
  def f = 42
  override def toString: String = super.toString
}

class BB {
  def f = 43
  override def toString: String = super.toString
}

class CC {
  def f = 44
  override def toString: String = super.toString
}

class A[E] extends java.util.Iterator[E] {
  override def next(): E = null.asInstanceOf[E]
  override def hasNext: Boolean = false
}

abstract class B[E] extends java.util.Iterator[E] {
  override def next(): E = null.asInstanceOf[E]
  override def hasNext(): Boolean = false
}

trait C[E] extends java.util.Iterator[E] {
  override def next(): E = null.asInstanceOf[E]
  override def hasNext(): Boolean = false
}
