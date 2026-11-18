import language.experimental.captureChecking
def launder[A <: AnyRef^, B <: AnyRef^](x: A & B): AnyRef = x   // error
def launder2[A <: AnyRef^](x: A): AnyRef = x   // error
def test(io: AnyRef^): AnyRef = launder(io)
def tracked(io: AnyRef^) =
  def f[A <: AnyRef^{io}, B <: AnyRef^{io}](x: A & B): AnyRef = x  // error
