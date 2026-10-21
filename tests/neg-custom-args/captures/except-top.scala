import language.experimental.captureChecking
import caps.*

// `Any` is the sole top of the classifier tree. `caps.Capability` is not a classifier
// class and is not the top either, so it is rejected as a projection argument.

def onlyCap(c: Object^): Object^{c.only[Capability]} = ??? // error
def exceptCap(c: Object^): Object^{c.except[Capability]} = ??? // error
