object i4471 {
  sealed trait Shuffle[A1, A2] {
    def andThen[A3](that: Shuffle[A2, A3]): Shuffle[A1, A3] = AndThen(this, that)
  }

  case class Id[A]() extends Shuffle[A, A]
  case class Swap[A, B]() extends Shuffle[(A, B), (B, A)]
  case class AssocLR[A, B, C]() extends Shuffle[((A, B), C), (A, (B, C))]
  case class AssocRL[A, B, C]() extends Shuffle[(A, (B, C)), ((A, B), C)]
  case class Par[A1, B1, A2, B2](_1: Shuffle[A1, B1], _2: Shuffle[A2, B2]) extends Shuffle[(A1, A2), (B1, B2)]
  case class AndThen[A1, A2, A3](_1: Shuffle[A1, A2], _2: Shuffle[A2, A3]) extends Shuffle[A1, A3]
  
  def rewrite3[A1, A2, A3, A4](
    op1: Shuffle[A1, A2],
    op2: Shuffle[A2, A3],
    op3: Shuffle[A3, A4]
  ): Option[Shuffle[A1, A4]] = (op1, op2, op3) match {
    case (
      _: Swap[x, y],
      _: AssocRL[u, v, w],
      op3_ : Par[p1, q1, p2, q2]
    ) => op3_ match {
      case Par(_: Swap[r, s], _: Id[p2_]) =>
        Some(
          AssocLR[v, w, u]() andThen Par(Id[v](), Swap[w, u]()) andThen AssocRL[v, u, w]()
        )
      case _ => None
    }
    case _ => None
  }
}
