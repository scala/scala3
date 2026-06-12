import caps.{any, Classifier, SharedCapability}

trait C extends SharedCapability, Classifier
trait D extends SharedCapability, Classifier

def f(c: Object^): Object^{c.except[C].except[D]} = ??? // error: at most one except clause per capture reference
