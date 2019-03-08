object simpleEQ {
  sealed trait EQ[A, B]
  final case class Refl[A](u: Unit) extends EQ[A, A]

  def conform[A, B](a: A, eq: EQ[A, B]): B = eq match {
    case Refl(()) => a
  }

  def conform2[A, B, C, D](a: A, b: B, eq: EQ[(A, B), (C, D)]): (C, D) =
    eq match {
      case Refl(()) => (a, b)
    }
}
