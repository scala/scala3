class C:
  def multi(x: Int, y: Int): Int = x + y
  def **(x: Int, y: Int): Int = x + y
  def g = new C() `multi` (x = 42, y = 27)
  def h = new C() ** (x = 42, y = 27)

type X = (x: Int)

class D(d: Int):
  def **(x: Int): Int = d * x
  def **(x: X): Int = d * x.x
  def f = this ** (x = 2)
  def g = this ** 2
  def h = this ** ((x = 2))
  def i = this ** (x = (1 + 1))
