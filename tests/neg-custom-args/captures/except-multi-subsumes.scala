import caps.{Classifier, Control, SharedCapability}

trait IOCap  extends SharedCapability, Classifier
trait NetCap extends IOCap, Classifier

def reverseMonotonicity(c: Object^): Unit =
  // dropping an exclusion yields a larger set, not a subset:
  // {c.except[Control]} </: {c.except[Control].except[IOCap]}
  val x: Object^{c.except[Control]}               = ???
  val y: Object^{c.except[Control].except[IOCap]} = x // error

def fullNotInDoubleExcept(c: Object^): Unit =
  // {c} </: {c.except[Control].except[IOCap]}
  val x: Object^{c}                               = ???
  val y: Object^{c.except[Control].except[IOCap]} = x // error

def excludedPartRejected(c: Object^): Unit =
  // the second exclusion removes exactly the IOCap part:
  // {c.only[IOCap]} </: {c.except[Control].except[IOCap]}
  val x: Object^{c.only[IOCap]}                   = ???
  val y: Object^{c.except[Control].except[IOCap]} = x // error
