//> using options -Xfatal-warnings -deprecation -feature

class C

object Givens:
  given cOrdering: Ordering[C]:
    override def compare(c0: C, c1: C) = 0
  val greeting = "we love Givens"