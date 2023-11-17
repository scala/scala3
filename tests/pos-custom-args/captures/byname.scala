import annotation.retainsByName
import caps.cap
class CC
type Cap = CC^

class I

def test(cap1: Cap, cap2: Cap): I^{cap1} =
  def f() = if cap1 == cap1 then I() else I()
  def h(x: ->{cap} I) = x
  h(f()) // OK
  def hh(x: -> I @retainsByName(cap1)) = x
  h(f())

