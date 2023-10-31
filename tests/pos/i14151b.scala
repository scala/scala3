class Single[B](val value: B):
  type Retrieve[A, T <: Single[? <: A]] <: A = T match { case Single[b] => b }
