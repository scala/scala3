package u

object Unpack:
  // def unapply(x: (a: Int, b: Int)): (a: Int, b: Int) = x // OK
  def unapply(x: (a: Int, b: Int)): x.type = x

def test(x: (a: Int, b: Int)) =
  x match
    case Unpack(b = n) => n
