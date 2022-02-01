@annotation.capability class Cap

def test(cap1: Cap, cap2: Cap) =
  def f() = if cap1 == cap1 then g else g
  def g(x: Int) = if cap2 == cap2 then 1 else x
  def g2(x: Int) = if cap1 == cap1 then 1 else x
  def f2() = if cap1 == cap1 then g2 else g2
  def h(ff: => {cap2} Int -> Int) = ff
  h(f())  // ok
  h(f2()) // error

class I

def test2(cap1: Cap, cap2: Cap): {cap1} I =
  def f() = if cap1 == cap1 then I() else I()
  def g() = if cap2 == cap2 then I() else I()
  def h(x: {cap1} -> I) = x // warning
  h(f()) // OK
  h(g()) // error



