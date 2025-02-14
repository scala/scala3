sealed trait T_A
case class CC_B[T](a: T) extends T_A

@main def test() = {
  val v_a: CC_B[Int] = CC_B(10)
  val v_b: Int = v_a match{
    case CC_B(12) => 0
  }
}
