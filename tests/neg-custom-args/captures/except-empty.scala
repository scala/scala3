import caps.{any, Classifier, SharedCapability}

trait C extends SharedCapability, Classifier
trait C1 extends C, Classifier

def test(c: Object^): Unit =
  val x: Object^{c.only[C1].except[C]} = ??? // error: the capture set is empty
