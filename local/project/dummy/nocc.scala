package dummy

trait NoCaptureChecking:
  def byName(f: => Int): Int
  def impure(f: Int => Int): Int
  def context(f: Int ?=> Int): Int
