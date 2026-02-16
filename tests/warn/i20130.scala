sealed trait T_A[B]
sealed trait T_B[C]
case class CC_B[C]() extends T_A[T_B[C]]
case class CC_C[B, C](c: T_A[B], d: T_B[C]) extends T_B[C]
case class CC_E[C]() extends T_B[C]

val v_a: T_B[Int] = CC_C(null, CC_E())
val v_b: Int = v_a match { // warn: not exhaustive
  case CC_C(_, CC_C(_, _)) => 0
  case CC_E()              => 5
}
