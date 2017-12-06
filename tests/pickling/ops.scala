object Test {

  val num = implicitly[Integral[Int]]
  val ops = num.mkOrderingOps
  ops(9) < 10

}
