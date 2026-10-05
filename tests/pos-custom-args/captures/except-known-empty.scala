import language.experimental.captureChecking
import caps.*

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier
trait D  extends SharedCapability, Classifier

class A[+T]

// c1 is classified as C1, which c1.only[C].except[C1] excludes, so the projection is empty.
def emptyByExclusion(c1: C1^) =
  val x: A[Unit]^{c1.only[C].except[C1]} = ???
  val y: A[Unit]^{} = x

// The projection becomes empty when a C1 capability is substituted for x.
def mk(x: Object^): A[Unit]^{x.only[C].except[C1]} = ???
def emptyAfterSubstitution(c1: C1^) =
  val y: A[Unit]^{} = mk(c1)

// The same, where the classifiers C1 and D of b come from its capture set.
def emptyByMixedBase(k1: C1^, d: D^) =
  val b: A[Unit]^{k1, d} = ???
  val y: A[Unit]^{} = mk(b)
