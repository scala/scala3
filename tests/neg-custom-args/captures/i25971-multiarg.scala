// Multiple parameters per arrow: each name should be preserved in the
// printed type that surfaces in the cc-time error.
import language.experimental.captureChecking
import caps.*

class IO extends caps.SharedCapability

object Test:
  val f = (a: IO, b: IO) => a
  val g: (IO, IO) => IO = f // error
