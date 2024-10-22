//> using options -deprecation
type X = (x: Int)

class E(e: Int):
  def **(x: Int): Int = e * x
  def **(x: X): Int = e * x.x
  def f = this ** (x = 2) // warn
