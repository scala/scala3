trait SetOps[A, +C <: SetOps[A, C]]  {
  def concat(that: Iterable[A]): C = ???
}

class Set1[A] extends SetOps // error: should be SetOps[A, Set1[A]]
