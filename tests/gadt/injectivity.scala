object injectivity {
  sealed trait EQ[A, B]
  final case class Refl[A](u: Unit) extends EQ[A, A]

  def conform[A, B, C, D](a: A, b: B, eq: EQ[(A, B), (C, D)]): C =
    eq match {
      case Refl(()) => a
    }
}
