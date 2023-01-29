import language.experimental.captureChecking
import scala.scalanative.safe.SafeZone

object SyntaxTest{

  class A (v: Int = 0) { self: {*} A =>
    override def toString(): String = s"A{ $v }"
  }

  class B (a: {*} A) { self: {a} B =>
    override def toString(): String = s"B{ $a }"
  }

  trait Trait{
    override def toString(): String = s"Trait{}"
  }

  // What follows `new {CaptureRef}` can not be a template body.
  def test(): Unit = {
    val sz: {*} SafeZone = SafeZone.open()
    val a: {*} A = new A(0)
    val x0 = new {} A(0) // error
    val str = "abc"
    val x1 = new {str} A(0) // error: str is not a safe zone
    val x2 = new {sz, sz} A(0) // error
    val x3: (Short -> Char) = new {sz} (Short -> Char) { def apply(x: Short) = x.toChar } // error // error
    val x4 = new {sz} { if a.toString().length > b.toString().length then a else b } // error // error
    val x5: {a} Trait = new B(a) with Trait { // error // error
      override def toString(): String = s"overrided: ${super.toString()}"
    }
  }
} // error