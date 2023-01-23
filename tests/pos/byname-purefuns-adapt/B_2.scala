import language.experimental.captureChecking
object B:
  def test(x: => Int) = A.f(x)


