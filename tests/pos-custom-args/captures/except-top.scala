import language.experimental.captureChecking
import caps.*

// `Any` is the top of the classifier tree. It is not a classifier class, but is accepted
// as a projection argument: `only[Any]` is the identity projection, `except[Any]` is empty.

def onlyTopIsIdentity(c: Object^): Unit =
  val a:  Object^{c.only[Any]} = ???
  val a1: Object^{c}           = a    // {c.only[Any]} <: {c}
  val a2: Object^{c.only[Any]} = a1   // ... and back: identity

def exceptTopIsEmpty(c: Object^): Unit =
  val a:  Object^{c.except[Any]} = ???
  val a1: Object                 = a  // pure: {c.except[Any]} =:= {}

// `except[Any]` drops only that element from the surrounding set
def exceptTopInSet(c: Object^, d: Object^): Unit =
  val x: Object^{c.except[Any], d} = ???
  val y: Object^{d}                = x  // {c.except[Any], d} =:= {d}
