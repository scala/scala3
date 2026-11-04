import language.experimental.captureChecking
import caps.*

trait C1 extends Classifier, SharedCapability
trait C2 extends Classifier, C1

class A extends SharedCapability

// Excluding C2 from a capability that already excludes C1 changes nothing, since C2 <: C1.
def f[c^ <: {any.except[C1]}](x: A^{c}): A^{c.except[C2]} = x

def g(x: A^{any.except[C1]}): A^{x.except[C2]} = x
