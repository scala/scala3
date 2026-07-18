//> using options -language:experimental.captureChecking -Werror
import caps.any
class C
type Cap = C^

object foo

def test(c: Cap, other: String): Unit =
  val x1: C^ = ??? // OK
  val x2: C^{other} = ??? // error: cs is empty
  val s1 = () => "abc"
  val x3: C^{s1} = ??? // error: cs is empty
  val x3a: () -> String = s1
  val s2 = () => if x1 == null then "" else "abc"
  val x4: C^{s2} = ??? // OK
  val x5: C^{c, c} = ??? // warn: redundant // warn: redundant
  // val x6: C^{c}^{c} = ??? // would be syntax error
  val x7: Cap^{c} = ??? // warn: redundant
  // val x8: C^{c}^{any} = ??? // would be syntax error
  val x9: C^{c, any}  = ??? // warn: redundant
  val x10: C^{any, c} = ??? // warn: redundant

  def even(n: Int): Boolean = if n == 0 then true else odd(n - 1)
  def odd(n: Int): Boolean = if n == 1 then true else even(n - 1)
  val e1 = even
  val o1 = odd

  val y1: String^{e1} = ??? // error cs is empty
  val y2: String^{o1} = ??? // error cs is empty

  lazy val ev: (Int -> Boolean) = (n: Int) =>
    lazy val od: (Int -> Boolean) = (n: Int) =>
      if n == 1 then true else ev(n - 1)
    if n == 0 then true else od(n - 1)
  val y3: String^{ev} = ??? // error cs is empty

  ()
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)
