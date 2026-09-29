import caps.{any, Classifier, Control, SharedCapability}

trait IOCap   extends SharedCapability, Classifier
trait NetCap  extends IOCap, Classifier
class Label   extends Control
class Shared1 extends SharedCapability   // classified exactly as SharedCapability

def reverseWidening(c: Object^): Unit =
  // {c} is not included in {c.except[IOCap]}
  val x: Object^{c}               = ???
  val y: Object^{c.except[IOCap]} = x // error

def antiMonotonicity(c: Object^): Unit =
  // excluding less is not a subset of excluding more:
  // {c.except[NetCap]} </: {c.except[IOCap]}
  val x: Object^{c.except[NetCap]} = ???
  val y: Object^{c.except[IOCap]}  = x // error

def unrelatedExclusions(c: Object^): Unit =
  // exclusions of unrelated classifiers do not subsume each other
  val x: Object^{c.except[Control]} = ???
  val y: Object^{c.except[IOCap]}   = x // error

def runOnNewThread[T](body: () ->{any.except[Control]} T): T = body()

def controlRejected(l: Label^): Unit =
  runOnNewThread: () =>
    println(l) // error: l is classified as Control

def unclassifiedRejected(o: Object^): Unit =
  runOnNewThread: () =>
    println(o) // error: o is unclassified, so it could capture Control capabilities

def ancestorRejected(s: Shared1^): Unit =
  // SharedCapability is an ancestor of Control, so a SharedCapability-classified
  // capability could still capture a Control capability and must be rejected.
  runOnNewThread: () =>
    println(s) // error
