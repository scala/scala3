package logs

object Test {
  val l = Logarithm(1.0)
  val l2 = Logarithm(2.0)
  val l3 = l * l2
  val l4 = l + l2
  val d = l3.toDouble
  val l5: Logarithm = (1.0).asInstanceOf[Logarithm]
}
