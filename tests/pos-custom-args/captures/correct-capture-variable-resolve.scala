import language.experimental.captureChecking

def f(x: AnyRef^, C: AnyRef^) =
  def g[C^ <: {x}](y: AnyRef^{C}): AnyRef^{x} = y