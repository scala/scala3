import language.experimental.captureChecking
import caps.{any, Classifier, SharedCapability, Control}

trait C   extends SharedCapability, Classifier
trait C1  extends C, Classifier
trait C11 extends C1, Classifier
trait D   extends SharedCapability, Classifier
class A

// Projections of two roots split an unclassified capability: both roots
// are instantiated with it, as `any` stands for a capture variable.
def twoRoots() =
  val x: A^ = A()
  val y: A^{any.only[C], any.except[C]} = x

// A projection of the base and a projection of a root share the work.
def baseAndRoot(x: A^) =
  val y: A^{x.only[C], any.except[C]} = x

def threeRoots() =
  val x: A^ = A()
  val y: A^{any.only[C].except[C1], any.only[C1], any.except[C]} = x

def holeInHole() =
  val x: A^ = A()
  val y: A^{any.except[C], any.only[C].except[C1], any.only[C1].except[C11], any.only[C11]} = x

// A classified argument is covered by the parts of its classifier.
def take(a: AnyRef^{any.only[C].except[C1], any.only[C1]}): Unit = ()
def classifiedArg(c: C^) = take(c)

// Capture-set bounds that are unions of root projections.
def run[X^ <: {any.only[C].except[C1], any.only[C1]}](): Unit = ()
def boundInst(c: C^) = run[{c}]()

// The expected capture set of a closure.
def runF(f: () ->{any.except[Control], any.only[Control]} Unit): Unit = ()
def closure(c: Object^): Unit = runF(() => println(c))

// Result types.
def result(consume x: A^): A^{any.only[C], any.except[C]} = x

// Read-only parts.
def readOnly() =
  val x: A^ = A()
  val src: A^{x.rd} = ???
  val y: A^{any.only[C].rd, any.except[C].rd} = src

// A capability is split after widening.
def widened(d: D^) =
  val x: A^ = A()
  val z: A^{x, d} = ???
  val y: A^{any.only[C], any.except[C]} = z

// Assignments. A shared capability has parts in C and outside of it.
def assign(s: SharedCapability^) =
  val x: A^{s} = ???
  var v: A^{any.only[C], any.except[C]} = A()
  v = x

// A root classified as C takes the C part that the projection of the base leaves.
def mk(x: AnyRef^): C^{x} = ???
def classifiedRootTakesPart() =
  val x: A^ = A()
  val c: C^{x.except[C], any} = mk(x)

// Self types use `caps.any` itself.
class Self(x: A^):
  this: Self^{any.only[C], any.except[C]} =>
  def f = x

