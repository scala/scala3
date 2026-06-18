import language.experimental.captureChecking
import caps.*

// `Any` and `caps.Capability` denote the top of the classifier tree. They are not
// classifier classes, but are accepted as projection arguments:
//   `only[top]` is the identity projection, `except[top]` is the empty projection.

def onlyTopIsIdentity(c: Object^): Unit =
  val a:  Object^{c.only[Any]}        = ???
  val a1: Object^{c}                  = a    // {c.only[Any]} <: {c}
  val a2: Object^{c.only[Any]}        = a1   // ... and back: identity
  val b:  Object^{c.only[Capability]} = ???
  val b1: Object^{c}                  = b
  val b2: Object^{c.only[Capability]} = b1

def exceptTopIsEmpty(c: Object^): Unit =
  val a:  Object^{c.except[Any]}        = ???
  val a1: Object                        = a    // pure: {c.except[Any]} =:= {}
  val b:  Object^{c.except[Capability]} = ???
  val b1: Object                        = b

// `except[top]` drops only that element from the surrounding set
def exceptTopInSet(c: Object^, d: Object^): Unit =
  val x: Object^{c.except[Any], d} = ???
  val y: Object^{d}                = x         // {c.except[Any], d} =:= {d}
