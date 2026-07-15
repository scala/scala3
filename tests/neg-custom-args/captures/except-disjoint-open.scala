import language.experimental.captureChecking
import caps.*

// Why we should avoid .forall in isKnownDisjointFrom:
trait Ctl extends Classifier, SharedCapability
class CanDo extends Ctl

def use(x: CanDo^): Unit = ()
def sink(x: AnyRef^{any.except[Ctl]}): Unit = ()

def test(ctl: CanDo^): Unit =
  class Foo:
    // consults Foo's capture set, which is still {} at this point, checking
    // with .forall is vacuously true
    def early(): Unit = sink(this)
    def late(): Unit = use(ctl) // error // now Foo's capture acquires ctl
  Foo().early()
