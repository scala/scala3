object UnCurryTest {

  def f(x: Int)(y: Int) = x + y

  f(1)(2)
}
