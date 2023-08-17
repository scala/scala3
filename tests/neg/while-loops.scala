import annotation.tailrec

object WhileLoops {
  def cond: Boolean = ???

  @tailrec def rec1: Unit = {
    while (cond) {
      rec1 // error: not in tail position
    }
  }

  @tailrec def rec2: Boolean = {
    while (rec2) { } // error: not in tail position
    true
  }
}
