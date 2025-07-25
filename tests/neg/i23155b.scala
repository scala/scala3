object Unpack_T {
  (1, 2) match {
    case Unpack_T(first, _) => first // error
  }
  def unapply(e: (Int, Int)): Some[Int *: Int *: EmptyTuple] = ???
}
