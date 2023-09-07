trait B:
  def f: Float = 1.0f

class A(override val f: Float) extends B

trait C extends B:
  abstract override val f = super.f + 100.0f

trait D extends B:
  abstract override val f = super.f + 1000.0f

class ACD10 extends A(10.0f) with C with D // error: parent trait D has a super call to method B.f, which binds to the value C.f. Super calls can only target methods.

object Test:
  def main(args: Array[String]): Unit =
    new ACD10 // was: NoSuchMethodError: 'float C.f$(C)'
