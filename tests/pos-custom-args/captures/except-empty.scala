import caps.{any, Classifier, SharedCapability}

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier

// An empty projection of a tracked ref denotes {} and is accepted; only retaining a
// genuinely untracked pure value is rejected. only[C1].except[C] is empty since C1 <: C.
def test(c: Object^): Unit =
  val x: Object^{c.only[C1].except[C]} = ???
  val y: Object                        = x   // {} <: pure
