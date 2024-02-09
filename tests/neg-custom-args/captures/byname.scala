@annotation.capability class Cap

def test(cap1: Cap, cap2: Cap) =
  def f() = if cap1 == cap1 then g else g // error
  def g(x: Int) = if cap2 == cap2 then 1 else x
  def g2(x: Int) = if cap1 == cap1 then 1 else x
  def f2() = if cap1 == cap1 then g2 else g2
  def h(ff: => Int ->{cap2} Int) = ff
  h(f())
  h(f2())

class I

def test2(cap1: Cap, cap2: Cap): I^{cap1} =
  def f() = if cap1 == cap1 then I() else I()
  def g() = if cap2 == cap2 then I() else I()
  def h(x: ->{cap1} I) = x // ok
  h(f()) // OK
  h(g()) // error



