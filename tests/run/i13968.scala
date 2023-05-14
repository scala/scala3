object Bar {
  def unapply(x: Any): Option[Int *: Int *: EmptyTuple] = Some(1 *: 2 *: Tuple())
}

object Bar23 {
  def unapply(x: Any): Option[
      Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *:
      Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *: Int *:
      Int *: Int *: Int *: EmptyTuple
  ] = Some(
     1 *:  2 *:  3 *:  4 *:  5 *:  6 *:  7 *:  8 *:  9 *: 10 *:
    11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *:
    21 *: 22 *: 23 *: Tuple()
  )
}

@main def Test() =
  "" match
    case Bar((a, b)) => assert(a == 1 && b == 2, (a, b))

  "" match
    case Bar23((
      u1, u2, u3, u4, u5, u6, u7, u8, u9, u10,
      u11, u12, u13, u14, u15, u16, u17, u18, u19, u20,
      u21, u22, u23
    )) => assert(u1 == 1 && u23 == 23, (u1, u23))
