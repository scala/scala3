// User-written parameter names should be preserved when CC reveals
// curried-dependent functions in error messages — including for the
// polymorphic case from the issue.
import language.experimental.captureChecking
import caps.*

class IO extends caps.SharedCapability

object Test:
  val f = [C^ <: {any}] => (xs: List[IO^{C}]) => (ys: IO^{C}) => (zs: IO^{C}) => { val _ = ys; () }
  val g: [C^ <: {any}] => List[IO^{C}] -> IO^{C} -> IO^{C} -> Unit = f // error
