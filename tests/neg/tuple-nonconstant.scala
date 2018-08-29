object Test {
  def cons[X, Xs <: Tuple](x: X, xs: Xs) = x *: xs // error: *: cannot be applied to tuple of unknown size
  def toArray[Xs <: Tuple](xs: Xs) = xs.toArray    // (second error is suppressed right now)
}