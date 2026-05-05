// Shadowed names: the inner closure rebinds `x`, both levels should print as
// `x` in the error message rather than synthetic names.
import language.experimental.captureChecking
import caps.*

class IO extends caps.SharedCapability

object Test:
  val f = (x: IO) => (x: IO) => x
  val g: IO => IO => IO = f // error
