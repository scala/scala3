import language.experimental.captureChecking
import scala.scalanative.safe.SafeZone

object PosTest {
   class A (v: Int = 0) { self: {*} A =>
    override def toString(): String = s"A{ $v }"
  }

  class B (a: {*} A) { self: {a} B =>
    override def toString(): String = s"B{ $a }"
  }

  class C (a0: {*} A, a1: {*} A) { self: {a0, a1} C =>
    override def toString(): String = s"C{ $a0, $a1 }"
  }

  trait Trait{
    override def toString(): String = s"Trait{}"
  }


  def basicNewExprTest(): Unit = {
    val a: {*} A = new A(0)
    val b: {a} B = new B(a)
    val x0: (Short -> Char) = new (Short -> Char) { def apply(x: Short) = x.toChar }
    val x1 = new { if a.toString().length > b.toString().length then a else b }
    val x2: {a} Trait = new B(a) with Trait {
      override def toString(): String = s"overrided: ${super.toString()}"
    }
    assert(x2.toString().equals("overrided: Trait{}"))
  }

  def testGivenSafeZone(sz: {*} SafeZone): Unit = {
    val a: {sz} A = new {sz} A(0)
    val ary: {sz} Array[A] = new {sz} Array[A](10)

    val aInHeap: {*} A = new A(0)

    val b0: {sz, aInHeap} B = new {sz} B(aInHeap) // {sz, aInHeap} B // important error
    val b1: {sz, a} B = new {sz} B(a) // {sz, a} B
    val b2: {sz} B = new {sz} B(a) // {sz, a} B

    val c: {sz, aInHeap} C = new {sz} C(a, aInHeap) // {sz, a, aInHeap} C
  }

  def testPos(): Unit = {
    val sz: {*} SafeZone = SafeZone.open()
    testGivenSafeZone(sz)

    SafeZone{ implicit sz => testGivenSafeZone(sz)}
  }
}