import caps.{Classifier, SharedCapability}

trait C extends SharedCapability, Classifier
trait D extends SharedCapability, Classifier

// multiple `except` clauses on the same capture reference
def f(c: Object^): Object^{c.except[C].except[D]} = ???
