class Ops[A](xs: List[A]):
  def map[B](x: A => B): List[B] = ???

extension [B](x: List[B])
  private def ops = new Ops[B](x)
  export ops.map  // `x` and `B` should not appear twice as a parameter

