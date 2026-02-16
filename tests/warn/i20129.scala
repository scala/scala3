sealed trait T_A[A]
case class CC_B[A](a: T_A[A], c: T_A[A]) extends T_A[Char]
case class CC_C[A]() extends T_A[A]
case class CC_G() extends T_A[Char]

val v_a: T_A[Char] = CC_B(CC_G(), CC_C())
val v_b: Int = v_a match { // warn: not exhaustive
  case CC_C()                   => 0
  case CC_G()                   => 1
  case CC_B(CC_B(_, _), CC_C()) => 2
  case CC_B(CC_C(), CC_C())     => 3
  case CC_B(_, CC_G())          => 4
  case CC_B(_, CC_B(_, _))      => 5
}
