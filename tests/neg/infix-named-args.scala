class C:
  def f = 42 + (x = 1) // error // werror
  def multi(x: Int, y: Int): Int = x + y
  def **(x: Int, y: Int): Int = x + y
  def g = new C() `multi` (x = 42, y = 27) // werror
  def h = new C() ** (x = 42, y = 27) // werror

type X = (x: Int)

class D(d: Int):
  def **(x: Int): Int = d * x
  def **(x: X): Int = d * x.x
  def f = this ** (x = 2) // werror
  def g = this ** 2
