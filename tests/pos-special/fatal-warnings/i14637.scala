class C

object Givens:
  given cOrdering: Ordering[C] with
    override def compare(c0: C, c1: C) = 0
  val greeting = "we love Givens"