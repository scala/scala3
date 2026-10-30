import language.experimental.captureChecking
import caps.*

class Ctl extends Control
class Shr extends SharedCapability

def test() =
  class Both:
    val c: Ctl^ = Ctl()
    val s: Shr^ = Shr()
    // The implied capability of `Both` combines the classifiers of `c` and `s`
    // to their least upper bound, SharedCapability. The inferred type of `s2`
    // is then covered by it. If the classifiers were combined to the subclass
    // Control instead, `s2` would be flagged as needing an explicit type.
    val s2 = Shr()
