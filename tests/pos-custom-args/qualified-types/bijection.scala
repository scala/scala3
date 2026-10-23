trait Bijection[A,B]:
  def f(a: A): B
  def g(b: B): A
  def gf_id(a: A): Unit with g(f(a)) == a
  def fg_id(b: B): Unit with f(g(b)) == b

class PlusMinusOne extends Bijection[Int, Int]:
  def f(a: Int): {b: Int with b == a + 1} = a + 1
  def g(b: Int): {a: Int with a == b - 1} = b - 1
  def gf_id(a: Int) = ()
  def fg_id(b: Int) = ()
