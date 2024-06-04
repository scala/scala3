class C1
class C2 extends C1, caps.Capability
class C3 extends C2

def test =
  val x1: C1 = new C1
  val x2: C1 = new C2 // error
  val x3: C1 = new C3 // error

  val y2: C2 = new C2
  val y3: C3 = new C3

  val z2: C1 = y2 // error

