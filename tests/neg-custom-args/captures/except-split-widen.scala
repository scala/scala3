import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier

// The widened remainder `d.only[C].except[C1]` <: {c.only[C].except[C1]} is not covered.
def uncoveredRemainder(c: Object^) =
  val d: Object^{c} = c
  val e: Object^{d.only[C1], c.except[C]} = d // error

// A projection of `d` does not cover the corresponding part of `c`.
def projectionOfSmaller(c: Object^) =
  val d: Object^{c} = c
  val e: Object^{d.only[C], c.except[C]} = c // error
