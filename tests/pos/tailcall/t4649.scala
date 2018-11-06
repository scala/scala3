import annotation.tailrec

object Test4649 {
  @tailrec
  def lazyFilter[E](s: Stream[E], p: E => Boolean): Stream[E] = s match {
    case h #:: t =>
      if (p(h)) h #:: lazyFilter(t, p) // Ok: not a tail call but in a closure
      else lazyFilter(t, p)
    case _ =>
      Stream.empty
  }

  def cond: Boolean = ???
  def foo(x: => Int): Int = ???

  @tailrec
  def bar: Int = {
    if (cond) foo(bar) // Ok: not a tail call but in a closure
    else bar
  }

  def foo2(x: Int => Int) = ???

  @tailrec
  def bar2: Int = {
    if (cond) foo2(_ => bar2) // Ok: not a tail call but in a closure
    else bar2
  }
}
