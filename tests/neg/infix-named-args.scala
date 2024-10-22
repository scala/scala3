class C:
  def f = 42 + (x = 1) // error // a named tuple!
  def multi(x: Int, y: Int): Int = x + y
  def **(x: Int, y: Int): Int = x + y
  def g = new C() `multi` (x = 42, y = 27) // werror // werror // not actually a tuple! appearances to the contrary
  def h = new C() ** (x = 42, y = 27) // werror // werror

type X = (x: Int)

class D(d: Int):
  def **(x: X): Int = d * x.x
  def f = this ** (x = 2)
  def g = this ** 2 // error
