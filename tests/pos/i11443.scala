enum Opt[T] {
  case Nn extends Opt[Nothing] with Comparable[Nn.type]

  def compareTo(nn: Nn.type) = 0
}