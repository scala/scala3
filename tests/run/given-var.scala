// Demonstrates that monomorphic givens are cached
object a:
  private var x = 1
  given Int = x
  def init(x: Int) = this.x = x
  def cur = summon[Int]

@main def Test =
  a.init(1)
  assert(a.cur == 1)
  a.init(2)
  assert(a.cur == 1)
