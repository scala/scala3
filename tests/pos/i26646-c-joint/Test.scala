object Test {
  val a: CyclicSignature.Actual[Nothing] = ???
  def x() = a.test()
}
