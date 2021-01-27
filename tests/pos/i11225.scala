import compiletime.uninitialized

class Memo[A](x: => A):
  private var cached1: A = uninitialized
  private var cached: A = uninitialized
  private var known: Boolean = false
  def force =
    if !known then
      known = true
      cached = x
      val y = cached1
    cached
