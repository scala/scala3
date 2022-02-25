class Single[B](val value: B):
  type Retrieve[A] <: A = Single[A] match { case Single[b] => b }
