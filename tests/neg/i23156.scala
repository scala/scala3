object Unpack {
  (1, 2) match {
    case Unpack(first, _) => first
  }
  def unapply(e: (Int, Int)): Option[T] = ??? // error
}