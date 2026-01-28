//> using options -Ycheck:expandPrivate

class C private (x: Int, y: Int = 1):
  def this() = this(1)
