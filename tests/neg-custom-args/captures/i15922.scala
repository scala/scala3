

trait Cap { def use(): Int }
type Id[X] = [T] -> (op: X => T) -> T
def mkId[X](x: X): Id[X] = [T] => (op: X => T) => op(x)

def withCap[X](op: (Cap^) => X): X = {
  val cap: Cap^ = new Cap { def use() = { println("cap is used"); 0 } }
  val result = op(cap)
  result
}

def leaking(c: Cap^): Id[Cap^{c}] = mkId(c)

def test =
  val ll = (c: Cap^) => leaking(c)
  val bad1 = withCap(ll)       // error
  val bad2 = withCap(leaking)  // error
