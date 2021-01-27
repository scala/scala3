import compiletime.notInitialized

class Memo[A](x: => A):
  private var cached1: A = notInitialized
  private var cached: A = notInitialized
  private var known: Boolean = false
  def force =
    if !known then
      known = true
      cached = x
      val y = cached1
    cached
