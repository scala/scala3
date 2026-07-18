trait EdgeLike[T] {
  def _1: T
  def _2: T
}

case class LabeledEdge[T, U](_1: T, label: U, _2: T) extends EdgeLike[T] // error
