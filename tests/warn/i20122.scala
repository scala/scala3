sealed trait T_B[C, D]

case class CC_A()
case class CC_B[A, C](a: A) extends T_B[C, CC_A]
case class CC_C[C, D](a: T_B[C, D]) extends T_B[Int, CC_A]
case class CC_E(a: CC_C[Char, Byte])

val v_a: T_B[Int, CC_A] = CC_B(CC_E(CC_C(null)))
val v_b = v_a match
  case CC_B(CC_E(CC_C(_))) => 0 // warn: unreachable
  case _                   => 1
    // for CC_B[A, C] to match T_B[C, CC_A]
    // C <: Int, ok
    // A <: CC_E, ok
    // but you need a CC_C[Char, Byte]
    // which requires a T_B[Char, Byte]
    // which isn't instantiable, outside of null
