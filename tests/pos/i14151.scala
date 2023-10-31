class BoundedPair[A, B <: A]:
  type Second[A, T <: BoundedPair[A, ? <: A]] <: A = T match { case BoundedPair[A, b] => b }
