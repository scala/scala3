import language.experimental.captureChecking
import caps.*

trait C1 extends Classifier, SharedCapability
trait C2 extends Classifier, C1

class A extends SharedCapability

def f(x: A^{any.except[C1]}): A^{x.except[C2]} = x
def g[c^ <: {any.except[C1]}](x: A^{c}): A^{c.except[C2]} = x
