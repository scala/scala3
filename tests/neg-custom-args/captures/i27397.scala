import language.experimental.captureChecking
import caps.*
class Ctl extends Control
class Ref extends ExclusiveCapability
def test() =
  class Both:
    val c: Ctl^ = Ctl()
    val r: Ref^ = Ref()
  val b: Both^{any.only[Control]} = Both()  // error
  class Single:
    val r: Ref^ = Ref()
  val s: Single^{any.only[Control]} = Single()  // error
