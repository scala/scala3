trait Cap { def use(): Int }
class Id[X](val value: [T] -> (op: X => T) -> T)
def mkId[X](x: X): Id[X] = Id([T] => (op: X => T) => op(x))

def bar() = {
  def withCap[X](op: (lcap: caps.Capability) ?-> Cap^{lcap} => X): X = {
    val cap: Cap = new Cap { def use() = { println("cap is used"); 0 } }
    val result = op(using caps.cap)(cap)
    result
  }

  val leak = withCap(cap => mkId(cap))
}