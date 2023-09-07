class B
type Cap = B^
class C0(n: Cap) // was error: class parameter must be a `val`, now OK

class C(val n: Cap):
  def foo(): B^{n} = n

def test(x: Cap, y: Cap) =
  val c0 = C(x)
  val c1: C = c0  // error
  val c2 = if ??? then C(x) else identity(C(y))
  val c3: C { val n: B^{x, y} }^{x} = c2  // error
