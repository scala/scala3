import scala.Function.applyUsing

class C

object Test {
  given C = new C

  val f23 = (
    a1: C, a2: C, a3: C, a4: C, a5: C, a6: C, a7: C, a8: C, a9: C, a10: C, a11: C, a12: C,
    a13: C, a14: C, a15: C, a16: C, a17: C, a18: C, a19: C, a20: C, a21: C, a22: C, a23: C) => 0

  def ok: String = ((c: C) => "ok").applyUsing // fixed-arity overload, not experimental
  def xxl: Int = f23.applyUsing // error: applyUsing for arity > 22 is experimental
}
