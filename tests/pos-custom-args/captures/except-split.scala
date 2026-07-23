import language.experimental.captureChecking
import caps.{Classifier, SharedCapability}

trait C  extends SharedCapability, Classifier
trait C1 extends C, Classifier

class A[+T]

def concreteSplit(c: Object^) =
  val src: A[Unit]^{c.only[C]}                          = ???
  val dst: A[Unit]^{c.only[C].except[C1], c.only[C1]}   = src

def f[X^](x: A[Unit]^{X.only[C].except[C1], X.only[C1]}): Unit = ???
def g[Y^](x: A[Unit]^{Y.only[C]}): Unit = f[Y](x)
