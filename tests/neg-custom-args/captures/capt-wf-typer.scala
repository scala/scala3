class C
type Cap = {*} C

object foo

def test(c: Cap, other: String): Unit =
  val x7: {c} String = ??? // OK
  val x8: String @retains(x7 + x7) = ??? // error
  val x9: String @retains(foo) = ??? // error
  ()