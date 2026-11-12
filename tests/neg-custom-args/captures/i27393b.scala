import language.experimental.captureChecking
def keep[A <: AnyRef^, B <: AnyRef^](x: A & B): AnyRef^ = x  // error // error
def keep2[A <: AnyRef^](x: A): AnyRef^ = x  // error // error
