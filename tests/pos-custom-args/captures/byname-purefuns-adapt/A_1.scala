class Ctx
object A:
  def f(x: => Int) = ()
  def g(op: Ctx ?=> Int) = op(using Ctx())
