object Test {
  def elem[Xs <: NonEmptyTuple](xs: Xs) = xs(1) // error: selection (...) cannot be applied to tuple of unknown size
}