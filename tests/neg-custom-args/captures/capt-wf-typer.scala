import annotation.retains
class C
type Cap = C^

object foo

def test(c: Cap, other: String): Unit =
  val x7: String^{c} = ??? // OK
  val x8: String @retains[""] = ??? // error
  val x9: String @retains[foo] = ??? // error
  ()