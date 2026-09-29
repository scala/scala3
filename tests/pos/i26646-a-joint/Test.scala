object Test {
  val a: CyclicSignature[Nothing] = ???
  def x() = a.test()
}
