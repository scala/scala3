import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

trait C extends SharedCapability, Classifier
trait C1 extends C, Classifier
trait C2 extends C, Classifier

class A[+T]

// Excluding C1 from an underlying C1 | C2 kind leaves a capability known to be C2.
def exclusionRefinesClassification(c1: Object^, c2: Object^) =
  val x: A[Unit]^{c1.only[C1], c2.only[C2]} = ???
  val src: A[Unit]^{x.except[C1]} = ???
  val dst: A[Unit]^{x.only[C2]} = src
