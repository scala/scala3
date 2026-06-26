import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier

class A[+T]

def noSplit(c: Object^) =
  val src: A[Unit]^{c.only[C]}              = ???
  val dst: A[Unit]^{c.only[C].except[C1]}   = src // error
