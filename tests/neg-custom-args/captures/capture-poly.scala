import caps.*

trait Foo extends Capability

trait CaptureSet:
  type C <: CapSet^

def capturePoly[C^](a: Foo^{C^}): Foo^{C^} = a
def capturePoly2(c: CaptureSet)(a: Foo^{c.C^}): Foo^{c.C^} = a

def test =
  val x: Foo^ = ???
  val y: Foo^ = ???

  object X extends CaptureSet:
    type C = CapSet^{x}

  val z1: Foo^{X.C^} = x
  val z2: Foo^{X.C^} = y // error

  val z3: Foo^{x} = capturePoly(x)
  val z4: Foo^{x} = capturePoly(y) // error

  val z5: Foo^{x} = capturePoly2(X)(x)
  val z6: Foo^{x} = capturePoly2(X)(y) // error