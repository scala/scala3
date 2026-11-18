import language.experimental.captureChecking
def pure[A <: AnyRef, B <: AnyRef](x: A & B): AnyRef = x
def mixed[A <: AnyRef^, B <: AnyRef](x: A & B): AnyRef = x
def tracked(io: AnyRef^) =
  def f[A <: AnyRef^{io}, B <: AnyRef^{io}](x: A & B): AnyRef^{io} = x
  def g[A <: AnyRef^{io}, B <: AnyRef](x: A & B): AnyRef = x
  f(io)
