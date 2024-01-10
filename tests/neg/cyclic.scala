//> using options -explain-cyclic
object O:
  def f() = g()
  def g() = h()
  def h() = i()
  def i() = f()  // error
