import language.experimental.captureChecking
import caps.{Control, Classifier, SharedCapability}

trait IOCap extends SharedCapability, Classifier

class Box[+T]
def excludeControl[T, D^](box: Box[T]^{D}): Box[T]^{D.except[Control]} = ???
def excludeIO[T, E^](box: Box[T]^{E}): Box[T]^{E.except[IOCap]} = ???

def test(w: Object^) =
  val w0: Box[Unit]^{w} = ???
  val r1 = excludeIO(w0)
  val r2 = excludeControl(r1)
  val chain: Box[Unit]^{w.except[Control].except[IOCap]} = r2
  val wide:  Box[Unit]^{w}                                = r2
