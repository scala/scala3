object complexEQ {
  sealed trait EQ[A, B]
  final case class Refl[A]() extends EQ[A, A]

  def m[A, B, C, D](e1: EQ[A, (B, C)], e2: EQ[A, (C, D)], d: D): A =
    e1 match {
      case Refl() => e2 match {
        case Refl() => 
          val r1: (B, B) = (d, d)
          val r2: (C, C) = r1
          val r3: (D, D) = r1
          r1
      }
    }

  def m2[Z, A, B, C, D](e0: EQ[Z, A], e1: EQ[A, (B, C)], e2: EQ[Z, (C, D)], d: D): Z =
    (e0, e1, e2) match {
      case (Refl(), Refl(), Refl()) =>
          val r1: (B, B) = (d, d)
          val r2: (C, C) = r1
          val r3: (D, D) = r1
          r1
    }
}
