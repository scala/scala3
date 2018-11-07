import annotation.tailrec

object WhileLoops {
  def cond: Boolean = ???

  @tailrec def rec1: Unit = { // error: tailrec not applicable
    while (cond) {
      rec1 // error: not in tail position
    }
  }

  @tailrec def rec2: Boolean = { // error: tailrec not applicable
    while (rec2) { } // error: not in tail position
    true
  }
}
