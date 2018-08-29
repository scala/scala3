object Test {
  def toArray[Xs <: Tuple](xs: Xs) = xs.toArray // error: toArray cannot be applied to tuple of unknown size
}