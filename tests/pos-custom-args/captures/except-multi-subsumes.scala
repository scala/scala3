import caps.{Classifier, Control, SharedCapability, Unscoped}

// Multiple exclusions on one capture reference. Hierarchy as in except-subsumes.scala:
//        SharedCapability        ExclusiveCapability
//         /          \                  |
//     Control       IOCap            Unscoped
//                     |
//                   NetCap
trait IOCap  extends SharedCapability, Classifier
trait NetCap extends IOCap, Classifier
class Uns    extends Unscoped

def monotonicity(c: Object^): Unit =
  // each added exclusion shrinks the set:
  // {c.except[Control].except[IOCap]} <: {c.except[Control]}, {c.except[IOCap]}, {c}
  val x:  Object^{c.except[Control].except[IOCap]} = ???
  val y1: Object^{c.except[Control]}               = x
  val y2: Object^{c.except[IOCap]}                 = x
  val y3: Object^{c}                               = x

def orderIndependent(c: Object^): Unit =
  // exclusion is a set, so order does not matter:
  // {c.except[Control].except[IOCap]} =:= {c.except[IOCap].except[Control]}
  val x: Object^{c.except[Control].except[IOCap]} = ???
  val y: Object^{c.except[IOCap].except[Control]} = x
  val z: Object^{c.except[Control].except[IOCap]} = y

def subclassRedundant(c: Object^): Unit =
  // excluding IOCap already drops the NetCap subtree, so excluding NetCap on top adds
  // nothing: {c.except[IOCap].except[NetCap]} =:= {c.except[IOCap]}
  val x:  Object^{c.except[IOCap].except[NetCap]} = ???
  val y:  Object^{c.except[IOCap]}                = x
  val x2: Object^{c.except[IOCap].except[NetCap]} = y

def disjointPartSurvives(u: Uns^): Unit =
  // Unscoped is on a different branch than Control and IOCap, so neither exclusion
  // removes it: {u} =:= {u.except[Control].except[IOCap]}
  val x: Object^{u.except[Control].except[IOCap]} = ???
  val y: Object^{u}                               = x
  val z: Object^{u.except[Control].except[IOCap]} = y

def onlyThenMultipleExcept(c: Object^): Unit =
  // {c.only[SharedCapability].except[Control].except[IOCap]} <: {c.only[SharedCapability].except[Control]}
  val x: Object^{c.only[SharedCapability].except[Control].except[IOCap]} = ???
  val y: Object^{c.only[SharedCapability].except[Control]}               = x
