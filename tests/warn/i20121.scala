sealed trait T_A[A, B]
type X = T_A[Byte, Byte]

case class CC_B[A](a: A) extends T_A[A, X]

val v_a: T_A[X, X] = CC_B(null)
val v_b = v_a match
  case CC_B(_) => 0
  case _       => 1 // warn: null only
    // for CC_B[A] to match T_A[X, X]
    // A := X
    // so require X, aka T_A[Byte, Byte]
    // which isn't instantiable, outside of null
