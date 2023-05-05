trait Cap { def use(): Int }
type Id[X] = [T] -> (op: X => T) -> T
def mkId[X](x: X): Id[X] = [T] => (op: X => T) => op(x)

def bar() = {
  def withCap[sealed X](op: (Cap^) => X): X = {
    val cap: Cap^ = new Cap { def use() = { println("cap is used"); 0 } }
    val result = op(cap)
    result
  }

  val leak = withCap(cap => mkId(cap))  // error
  leak { cap => cap.use() }
}