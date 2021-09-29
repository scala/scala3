class C
type Cap = {*} C

object foo

def test(c: Cap, other: String): Unit =
  val x1: {*} C = ??? // OK
  val x2: {other} C = ??? // error: cs is empty
  val s1 = () => "abc"
  val x3: {s1} C = ??? // error: cs is empty
  val x3a: () => String = s1
  val s2 = () => if x1 == null then "" else "abc"
  val x4: {s2} C = ??? // OK
  val x5: {c, c} C = ??? // error: redundant
  val x6: {c} {c} C = ??? // error: redundant
  val x7: {c} Cap = ??? // error: redundant
  val x8: {*} {c} C = ??? // OK
  val x9: {c, *} C = ??? // error: redundant
  val x10: {*, c} C = ??? // error: redundant

  def even(n: Int): Boolean = if n == 0 then true else odd(n - 1)
  def odd(n: Int): Boolean = if n == 1 then true else even(n - 1)
  val e1 = even
  val o1 = odd

  val y1: {e1} String = ??? // error cs is empty
  val y2: {o1} String = ??? // error cs is empty

  lazy val ev: (Int => Boolean) = (n: Int) =>
    lazy val od: (Int => Boolean) = (n: Int) =>
      if n == 1 then true else ev(n - 1)
    if n == 0 then true else od(n - 1)
  val y3: {ev} String = ??? // error cs is empty

  ()