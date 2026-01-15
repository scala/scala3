class C1
class C2 extends C1, caps.SharedCapability
class C3 extends C2

def test =
  val x1: C1 = new C1
  val x2: C1 = new C2 // was error, now ok
  val x3: C1 = new C3 // was error, now ok

  val y2: C2 = new C2
  val y3: C3 = new C3
  val y2ok: C2^{} = new C2

  val z2: C1 = y2 // error
  val z2ok: C1 = y2ok

