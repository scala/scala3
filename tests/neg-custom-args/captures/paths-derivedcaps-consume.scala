import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

class Foo extends ExclusiveCapability, Classifier

// Testing with various DerivedCapabilities
class D
class C(val d: D)
class B(val c: C) extends Foo, Stateful: // error
  update def foo() = println("foo")
