import language.experimental.captureChecking
import caps.{any, Classifier, SharedCapability}

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier
trait C11 extends C1, Classifier
trait C2 extends C, Classifier
trait C3 extends C, Classifier
trait C4 extends C, Classifier
trait C5 extends C, Classifier
trait C6 extends C, Classifier
trait C7 extends C, Classifier
trait C8 extends C, Classifier
trait D extends SharedCapability, Classifier

class A[+T]

def concreteSplit(c: Object^) =
  val src: A[Unit]^{c.only[C]}                          = ???
  val dst: A[Unit]^{c.only[C].except[C1], c.only[C1]}   = src

def concreteSplitReverse(c: Object^) =
  val src: A[Unit]^{c.only[C]}                          = ???
  val dst: A[Unit]^{c.only[C1], c.only[C].except[C1]}   = src

def topSplit(c: Object^) =
  val src: A[Unit]^{c}                                 = ???
  val dst: A[Unit]^{c.except[C1], c.only[C1]}           = src

def classifiedRawSplit(c: C^) =
  val src: A[Unit]^{c}                                 = ???
  val dst: A[Unit]^{c.only[C].except[C1], c.only[C1]}   = src

def readOnlySplit(c: Object^) =
  val src: A[Unit]^{c.only[C].rd} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1].rd,
    c.only[C1].rd
  } = src

def multiSplit(c: Object^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1].except[C2],
    c.only[C1],
    c.only[C2]
  } = src

def splitAlreadyExcludedSource(c: Object^) =
  val src: A[Unit]^{c.only[C].except[C2]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1].except[C2],
    c.only[C1]
  } = src

def nestedSplit(c: Object^) =
  val src: A[Unit]^{c.only[C].except[C11]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1],
    c.only[C1].except[C11]
  } = src

def splitIgnoresDisjointPeer(c: Object^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1],
    c.only[C1],
    c.only[D]
  } = src

def manySplit(c: Object^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C].except[C1].except[C2].except[C3].except[C4]
      .except[C5].except[C6].except[C7].except[C8],
    c.only[C1], c.only[C2], c.only[C3], c.only[C4],
    c.only[C5], c.only[C6], c.only[C7], c.only[C8]
  } = src

// The source region narrows to the base's classifiers: c is C1-classified, so
// {c.only[C]} covers only the C1 subtree.
def narrowedSplit(c: C1^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{
    c.only[C1].except[C11],
    c.only[C11]
  } = src

def narrowedSinglePeer(c: C1^) =
  val src: A[Unit]^{c.only[C]} = ???
  val dst: A[Unit]^{c.only[C1]} = src

def f[X^](x: A[Unit]^{X.only[C].except[C1], X.only[C1]}): Unit = ???
def g[Y^](x: A[Unit]^{Y.only[C]}): Unit = f[Y](x)

// A read-only peer and a writable peer together cover a read-only source.
def mixedReadOnlyPeers(c: Object^) =
  val src: A[Unit]^{c.only[C].rd} = ???
  val dst: A[Unit]^{c.only[C].except[C1].rd, c.only[C1]} = src

def splitBoundedParam[X^ <: {any.only[C1], any.only[C2]}](src: A[Unit]^{X})
    : A[Unit]^{X.only[C1], X.only[C2]} = src

// Instantiating X with a root splits that root.
def splitRoot[X^](f: A[Unit]^{X} => A[Unit]^{X.except[C1], X.only[C1]}) = ()
def useSplitRoot() = splitRoot[{any}](z => z)

// The selection rule narrows h.m to the captures of h, also when the capture set
// of m is split into projections.
class Split[X^, Y^]:
  def m: A[Unit]^{X.only[C].except[C1], X.only[C1], Y} = ???
def narrowSplit(c: Object^, d: Object^) =
  val h: Split[{c}, {d}]^{c.only[C]} = ???
  val r: A[Unit]^{c.only[C]} = h.m
