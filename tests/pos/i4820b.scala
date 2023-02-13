trait SetOps[A, +C <: SetOps[A, C]]  {
  def concat(that: Iterable[A]): C = ???
}

class Set1[A] extends SetOps // ideally should be SetOps[A, Set1[A]], but SetOps[Nothing, Nothin] is inferred
