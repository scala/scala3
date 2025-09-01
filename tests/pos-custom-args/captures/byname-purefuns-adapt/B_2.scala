import language.experimental.captureChecking
object B:
  def test(x: => Int) = A.f(x)
  def opp(using Ctx) = 0
  A.g(opp)



