class CC
type Cap = {*} CC

class I

def test(cap1: Cap, cap2: Cap): {cap1} I =
  def f() = if cap1 == cap1 then I() else I()
  def h(x: /*=>*/ {cap1} I) = x  // TODO: enable cbn
  h(f())

