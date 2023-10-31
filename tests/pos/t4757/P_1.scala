trait S[T]

object P {
  def x(t: Int)(ss: Seq[S[?]]): Seq[S[?]] = ss
}

