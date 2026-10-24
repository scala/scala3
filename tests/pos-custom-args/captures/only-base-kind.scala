import caps.{any, Classifier, SharedCapability, ExclusiveCapability}

// A projection is classified by the meet of its classifiers with the classifiers
// of the underlying capability: with `x: C1^` and `C1 <: C`, `x.only[C]` is a `C1`.

trait C extends SharedCapability, Classifier
trait C1 extends C, Classifier
trait C11 extends C1, Classifier
trait C2 extends C, Classifier
trait D extends SharedCapability, Classifier
class K1 extends C1
class A

def take(a: A^{any.only[C1]}): Unit = ()
def takeC2(a: A^{any.only[C2]}): Unit = ()
def takeD(a: A^{any.only[D]}): Unit = ()
def takeNotC1(a: A^{any.except[C1]}): Unit = ()
def takeRd(a: A^{any.only[C1].rd}): Unit = ()
def runC1(f: () ->{any.only[C1]} Unit): Unit = ()

// into a projection of a different base
def rootTarget(x: C1^) =
  val y: A^{x.only[C]} = ???
  take(y)

// into a capture set variable classified as C1
def mk(x: C1^): C1^{x.only[C]} = ???
def classifiedVar(x: C1^) =
  val v = mk(x)
  ()

// the unrelated part D of the base is dropped by the restriction
def mixedBase(c1: C1^, d: D^)(a: A^{c1, d}) =
  val y: A^{a.only[C]} = ???
  take(y)

def mk2(c1: C1^, d: D^)(a: A^{c1, d}): C1^{a.only[C]} = ???
def mixedBaseVar(c1: C1^, d: D^)(a: A^{c1, d}) =
  val v = mk2(c1, d)(a)
  ()

// an exclusion that removes a whole classifier of the base drops it
def exclusionDropsPart(c1: C1^, c2: C2^)(a: A^{c1, c2}) =
  val y: A^{a.only[C].except[C2]} = ???
  take(y)

def exceptOnly(c1: C1^, c2: C2^)(a: A^{c1, c2}) =
  val y: A^{a.except[C2]} = ???
  take(y)

// the variable keeps `a.except[C2]` instead of widening it to `c1.except[C2]`
def mk3(c1: C1^, c2: C2^)(a: A^{c1, c2}): C1^{a.except[C2]} = ???
def exceptVar(c1: C1^, c2: C2^)(a: A^{c1, c2}) =
  val v = mk3(c1, c2)(a)
  val w: C1^{a.except[C2]} = v

// a projection that removes every classifier of the base is empty
def emptyMeet(x: C1^) =
  val y: A^{x.except[C1]} = ???
  takeD(y)

// a projection of a reference whose capture set is a projection
def nested(x: C1^) =
  val y: A^{x.only[C]} = ???
  val z: A^{y.only[C]} = ???
  take(z)

def readOnly(x: C1^) =
  val y: A^{x.only[C].rd} = ???
  takeRd(y)

def inferredLocal(x: C1^) =
  val y: A^{x.only[C]} = ???
  val w = y
  take(w)

def closure(x: C1^) =
  val y: A^{x.only[C]} = ???
  val f: () ->{any.only[C1]} Unit = () => { val _ = y; () }
  runC1(() => { val _ = y; () })

// capture set parameters, whose bound gives their classifiers
def setBound[X^ <: {any.only[C1]}](a: A^{X.only[C]}) = take(a)

def termBound(x: C1^) =
  def h[X^ <: {x}](a: A^{X.only[C]}): Unit = take(a)
  ()

// a mixed bound: the projections keep only C1, respectively only C2 parts
def exceptOnlyBound[X^ <: {any.only[C1], any.only[C2]}](a: A^{X.except[C2]}) = take(a)

def disjointMeet[X^ <: {any.only[C11], any.only[C2]}](a: A^{X.only[C].except[C11]}) =
  takeNotC1(a)
  takeC2(a)

// a local root classified by the class of a new instance
def proj(x: Object^): A^{x.only[C]} = ???
def newInstance() =
  val y = proj(K1())
  take(y)
  take(proj(K1()))

// fields of classified classes
trait T extends C1:
  val m: A^{this.only[C]} = ??? // `this` is a C1
  val n: A^{this.only[D]} = ??? // empty

def localClass(c1: C1^, c2: C2^)(a: A^{c1, c2}) =
  class K extends C1:
    val f: A^{a.except[C2]} = ???
    val g: A^{a.only[C].except[C2]} = ???
  ()

// projections whose base is not yet fully known are not cached
trait E extends ExclusiveCapability, Classifier
def pair(a: A^, b: A^): Unit = ()
def wrapC1(x: Object^): C1^{x} = ???
def projE(x: Object^): A^{x.only[E]} = ???
def withVar[X^](x: A^{X}, g: A^{X} => () ->{X.only[C]} Unit): Unit = ()

def rootClassifiedLater(c1: C1^) =
  val x: A^{c1} = A()
  val y: A^{any.except[D]} = x
  val k = wrapC1(y) // classifies the root of `y` as C1, so `y` is shared
  pair(y, y)

def varSolvedLater(c: C^) =
  val y: A^{c} = ???
  withVar(y, a => { val z: A^ = projE(a); () => () }) // `a.only[E]` is empty
