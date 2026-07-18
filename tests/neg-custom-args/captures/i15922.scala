

trait Cap { def use(): Int }
class Id[+X](val value: [T] -> (op: X => T) -> T)
def mkId[X](x: X): Id[X] = Id([T] => (op: X => T) => op(x))

def withCap[X](op: (Cap^) => X): X = {
  val cap: Cap^ = new Cap { def use() = { println("cap is used"); 0 } }
  val result = op(cap)
  result
}

def leaking(c: Cap^): Id[Cap^{c}] = mkId(c)

def test =
  val ll = (c: Cap^) => leaking(c)  // was error, now ok
  val bad1 = withCap(ll)       // error, was ok
  val bad2 = withCap(leaking)  // error
