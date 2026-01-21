import caps.*
class Ref extends Stateful, Unscoped

class C:
  val r: Ref = Ref()

def test =
  val c = C()
  val _: C^{any.rd} = c
  val _: C^{any.only[Unscoped]} = c
  val _: C^{any.only[Unscoped].rd} = c
  val _: C^{} = c // error
