sealed trait T_A[A]
case class CC_B[A](a: T_A[A]) extends T_A[Byte]
case class CC_E[A](b: T_A[A]) extends T_A[Byte]

val v_a: T_A[Byte] = CC_E(CC_B(null))
val v_b: Int = v_a match { // warn: not exhaustive
  case CC_E(CC_E(_)) => 0
  case CC_B(_)       => 1
}
