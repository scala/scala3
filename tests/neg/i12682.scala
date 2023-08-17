//> using options -explain

object C:
  def m(x: Int) = 1
  object T extends K:
    val x = m(1)  // error
class K:
  def m(i: Int) = 2
object X extends K
object Y extends K
object D:
  import X.*, Y.*
  def d = m(42) // error
