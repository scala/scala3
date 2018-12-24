object injectivity {
  sealed trait EQ[A, B]
  final case class Refl[A]() extends EQ[A, A]

  def conform[A, B, C, D](a: A, b: B, eq: EQ[(A, B), (C, D)]): C =
    eq match {
      case _: Refl[a] =>
        val ab: (A, B) = (a, b)
        val cd: (C, D) = ab
        val rab: a = ab
        val rcd: a = cd
        a
    }
}
