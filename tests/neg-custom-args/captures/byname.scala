@annotation.capability class Cap

def test(cap1: Cap, cap2: Cap) =
  def f() = if cap1 == cap1 then g else g
  def g(x: Int) = if cap2 == cap2 then 1 else x
  def h(ff: => {cap2} Int -> Int) = ff
  h(f())  // error

class I

def test2(cap1: Cap, cap2: Cap): {cap1} I =
  def f() = if cap1 == cap1 then I() else I()
  def g() = if cap2 == cap2 then I() else I()
  def h(x: {cap1} -> I) = x // warning
  h(f()) // OK
  h(g()) // error



