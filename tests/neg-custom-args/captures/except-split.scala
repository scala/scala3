import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier
trait C11 extends C1, Classifier
trait C2 extends C, Classifier

class A[+T]

def noSplit(c: Object^) =
  val src: A[Unit]^{c.only[C]}              = ???
  val dst: A[Unit]^{c.only[C].except[C1]}   = src // error

def narrowFillerDoesNotCoverHole(c: Object^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1],
    c.only[C11]
  } = src // error

def excludedFillerLeavesItsHole(c: Object^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1],
    c.only[C1].except[C11]
  } = src // error

def missingOneOfSeveralHoles(c: Object^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1].except[C2],
    c.only[C1]
  } = src // error

def readOnlyPiecesDoNotCoverWritable(c: Object^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1].rd,
    c.only[C1].rd
  } = src // error

// Narrowing {c.only[C]} to c's C1 kind must not overshoot: the region is the
// full C1 subtree, not less.
def narrowedRootNotInPeer(c: C1^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{c.only[C11]} = src // error

def narrowedMissingHole(c: C1^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{c.only[C1].except[C11]} = src // error
