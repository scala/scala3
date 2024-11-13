sealed trait T_A[A, B]
sealed trait T_B[C]

case class CC_D[A, C]() extends T_A[A, C]
case class CC_E() extends T_B[Nothing]
case class CC_G[A, C](c: C) extends T_A[A, C]

val v_a: T_A[Boolean, T_B[Boolean]] = CC_G(null)
val v_b = v_a match {
  case CC_D()  => 0
  case CC_G(_) => 1 // warn: unreachable
    // for CC_G[A, C] to match T_A[Boolean, T_B[Boolean]]
    // A := Boolean, which is ok
    // C := T_B[Boolean],
    // which isn't instantiable, outside of null
}
