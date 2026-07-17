// Example 3 from OOPSLA 26

trait Vec[T]:
  def len: Int
  def concat(b: Vec[T]): {r: Vec[T] with r.len == this.len + b.len}
  def zip[S](b: Vec[S] with b.len == this.len): {r: Vec[(T, S)] with r.len == this.len}

def example3(
  n: Int,
  m: Int,
  v1: {v: Vec[Int] with v.len == n},
  v2: {v: Vec[Int] with v.len == m},
  v3: {v: Vec[String] with v.len == n + m}
): {r: Vec[(String, Int)] with r.len == m + n} =
  v3.zip(v1.concat(v2))
