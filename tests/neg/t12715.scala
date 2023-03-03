trait A              { def f: String }
trait B extends A    { def f = "B" }
trait C extends A    { override val f = "C" }
trait D extends C    { override val f = "D" }
trait E extends A, B { def d = super.f }
final class O1 extends B, C, D, E // error: parent trait E has a super call which binds to the value D.f. Super calls can only target methods.
final class O2 extends B, C, E, D // error: parent trait E has a super call which binds to the value C.f. Super calls can only target methods.
final class O3 extends B, E, C, D

object Main:
  def main(args: Array[String]): Unit =
    println(O1().f) // D
    println(O2().f) // D
    println(O3().f) // D
    println(O3().d) // B
    O1().d // was: NoSuchMethodError: 'java.lang.String D.f$(D)'
    O2().d // was: NoSuchMethodError: 'java.lang.String C.f$(C)'
