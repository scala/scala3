class Client {
  val a = (_: Bar_1.A[AnyRef]).foo()
  val b = (_: Bar_1.B[AnyRef]).bar()
  def test(x: Bar_1.A[AnyRef]): Bar_1.B[_] = x
}
