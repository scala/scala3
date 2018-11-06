import annotation.tailrec

class UnrolledBuffer {
  def remove(idx: Int): Unit = ()
  @tailrec final def remove(idx: Int, count: Int): Unit =
    if (count > 0) {
      remove(idx) // ok: not a recursive call
      remove(idx, count - 1)
    }
}
