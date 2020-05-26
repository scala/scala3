object Test {
  type DU[A <: Tuple] <: Tuple = A match {
    case EmptyTuple => EmptyTuple
    case Unit *: tl => DU[tl]
    case hd *: tl => hd *: DU[tl]
  }

  (1, 2): DU[Int *: Int *: EmptyTuple]
}
