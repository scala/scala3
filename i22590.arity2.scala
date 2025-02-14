sealed trait T_B
case class CC_A() extends T_B
case class CC_C() extends T_B

sealed trait T_A
case class CC_B[B](a: B,b:T_B) extends T_A


@main def test() = {
  val v_a: CC_B[Int] = null
  val v_b: Int = v_a match {
    case CC_B(12, CC_A()) => 0
    case CC_B(_, CC_C()) => 0
  }
}
