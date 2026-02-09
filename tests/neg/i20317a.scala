type SemigroupStructural[A] =
  A & { def combine(a: A): A }
def combineAll[A <: SemigroupStructural[A]](
  i: A, l: List[A]  // error
): A = l.foldLeft(i)(_.combine(_)) // error
