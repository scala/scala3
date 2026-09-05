// Example 3 from OOPSLA 26

type Vec[T]
object Vec:
  def fill[T](n: Int, v: T): {r: Vec[T] with r.len == n} = ???
extension [T](a: Vec[T])
  def len: Int = ???
  def concat(b: Vec[T]): {r: Vec[T] with r.len == a.len + b.len} = ???
  def zip[S](b: Vec[S] with b.len == a.len): {r: Vec[(T, S)] with r.len == a.len} = ???
def example3(n: Int, m: Int): {r: Vec[(String, Int)] with r.len == m + n} =
  val v1 = Vec.fill(n, 0)
  val v2 = Vec.fill(m, 1)
  val v3 = v1.concat(v2)
  val mPlusN = m + n // inferred as Int
  Vec.fill(mPlusN, "").zip(v3)
