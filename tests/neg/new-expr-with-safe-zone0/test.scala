import language.experimental.captureChecking
import scala.scalanative.safe.SafeZone

object CCTypeMatchTest {

  class A (v: Int = 0) { self: {*} A => }

  class B (a: {*} A) { self: {a} B => }

  class C (a0: {*} A, a1: {*} A) { self: {a0, a1} C => }

  def test(): Unit = {
    val sz: {*} SafeZone = SafeZone.open()

    val a: A = new {sz} A(0) // error: {sz} A
    val ary: Array[A] = new {sz} Array[A](10) // error: {sz} Array[A]

    val aInHeap: {*} A = new A(0)
    val b: {sz} B = new {sz} B(aInHeap) // error: {sz, aInHeap} B

    val aInZone: {*} A = new {sz} A(0)
    val c: {sz} C = new {sz} C(aInZone, aInHeap) // error: {sz, aInHeap} C
  }
}