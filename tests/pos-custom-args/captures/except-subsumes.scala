import caps.{any, Classifier, Control, SharedCapability}

trait C extends SharedCapability, Classifier
trait C1 extends C, Classifier

class Label extends Control

def test(c: Object^): Unit =
  val x1: Object^{c.except[C1]} = ???
  val y1: Object^{c} = x1            // {c.except[C1]} <: {c}
  val x2: Object^{c.only[C].except[C1]} = ???
  val y2: Object^{c.only[C]} = x2    // {c.only[C].except[C1]} <: {c.only[C]}
  val y3: Object^{c} = x2            // ... and transitively {c.only[C].except[C1]} <: {c}
  val x3: Object^{c.except[C1].rd} = ???
  val y4: Object^{c.rd} = x3         // {c.except[C1].rd} <: {c.rd}
  val x4: Object^{c.except[C]} = ???
  val y5: Object^{c.except[C1]} = x4 // {c.except[C]} <: {c.except[C1]} since C1 extends C

def testEmpty(l: Label^): Unit =
  // The exclusion removes everything `l` could capture, so the
  // capture set {l.except[Control]} is known to be empty.
  val x: Object^{l.except[Control]} = ???
  val y: Object = x
