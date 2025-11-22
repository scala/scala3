import caps.*
class Ref extends Mutable, Unscoped

class C:
  val r: Ref = Ref()

def test =
  val c = C()
  val _: C^{cap.rd} = c
  val _: C^{cap.only[Unscoped]} = c
  val _: C^{cap.only[Unscoped].rd} = c
  val _: C^{} = c // error
