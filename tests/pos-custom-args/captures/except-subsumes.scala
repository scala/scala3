import caps.{any, Classifier, Control, SharedCapability, Unscoped}

// A classifier hierarchy:   SharedCapability        ExclusiveCapability
//                            /          \                  |
//                        Control       IOCap            Unscoped
//                                        |
//                                      NetCap
trait IOCap  extends SharedCapability, Classifier
trait NetCap extends IOCap, Classifier
class Label  extends Control
class Net    extends NetCap
class Uns    extends Unscoped

def widening(c: Object^): Unit =
  // {c.except[C]} <: {c}
  val x1: Object^{c.except[IOCap]} = ???
  val y1: Object^{c}               = x1

def monotonicity(c: Object^): Unit =
  // excluding a larger subtree yields a smaller capture set:
  // {c.except[IOCap]} <: {c.except[NetCap]}   since NetCap extends IOCap
  val x: Object^{c.except[IOCap]}  = ???
  val y: Object^{c.except[NetCap]} = x

def onlyThenExcept(c: Object^): Unit =
  // {c.only[IOCap].except[NetCap]} <: {c.only[IOCap]} <: {c}
  val x: Object^{c.only[IOCap].except[NetCap]} = ???
  val y: Object^{c.only[IOCap]}                = x
  val z: Object^{c}                            = x

def readOnlyComposition(c: Object^): Unit =
  // {c.except[IOCap].rd} <: {c.rd}
  val x: Object^{c.except[IOCap].rd} = ???
  val y: Object^{c.rd}               = x

def unrelatedExclusionIsVacuous(c: Object^): Unit =
  // Control is unrelated to IOCap, so excluding it from c.only[IOCap] removes
  // nothing: {c.only[IOCap].except[Control]} =:= {c.only[IOCap]}.
  val a: Object^{c.only[IOCap]}                 = ???
  val b: Object^{c.only[IOCap].except[Control]} = a
  val c2: Object^{c.only[IOCap]}                = b

def excludingAnAncestor(l: Label^): Unit =
  // Label is a Control, which extends SharedCapability; excluding SharedCapability
  // removes everything Label could be, so {l.except[SharedCapability]} is empty.
  val x: Object^{l.except[SharedCapability]} = ???
  val y: Object                              = x

def unscopedIsDisjointFromShared(u: Uns^): Unit =
  // Unscoped and SharedCapability are unrelated branches, so the exclusion keeps u.
  val x: Object^{u.except[SharedCapability]} = ???
  val y: Object^{u}                          = x
