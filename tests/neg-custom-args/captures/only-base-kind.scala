import caps.{any, Classifier, SharedCapability}

trait C extends SharedCapability, Classifier
trait C1 extends C, Classifier
trait C11 extends C1, Classifier
trait C2 extends C, Classifier
trait D extends SharedCapability, Classifier
class A

def take(a: A^{any.only[C1]}): Unit = ()
def takeC2(a: A^{any.only[C2]}): Unit = ()
def takeNotC1(a: A^{any.except[C1]}): Unit = ()
def runC1(f: () ->{any.only[C1]} Unit): Unit = ()

// the base is only known to be a C
def baseAbove(x: C^) =
  val y: A^{x.only[C]} = ???
  take(y) // error

// the base has a C2 part, which the restriction keeps
def mixedBase(c1: C1^, c2: C2^)(a: A^{c1, c2}) =
  val y: A^{a.only[C]} = ???
  take(y) // error

// an unclassified base
def unclassified(a: A^) =
  val y: A^{a.only[C]} = ???
  take(y) // error

// the meet C1 is unrelated to the target C2
def unrelatedTarget(x: C1^) =
  val y: A^{x.only[C]} = ???
  takeC2(y) // error

// an exclusion leaves C minus C1, which is not a C2
def holedBase(c1: C1^, c: C^)(a: A^{c1, c}) =
  val y: A^{a.only[C].except[C1]} = ???
  takeC2(y) // error

// an exclusion that removes only part of a classifier keeps it
def exceptPart[X^ <: {any.only[C]}](a: A^{X.except[C2]}) = take(a) // error

// capture set variable classified as C1, base only known to be a C
def mk(x: C^): C1^{x.only[C]} = ???
def classifiedVar(x: C^) =
  val v = mk(x) // error
  ()

def boundAbove[X^ <: {any.only[C]}](a: A^{X.only[C]}) = take(a) // error

// the meet [C1, C2] minus C11 still has a C1 part
def notDisjoint[X^ <: {any.only[C1], any.only[C2]}](a: A^{X.only[C].except[C11]}) =
  takeNotC1(a) // error

def closure(x: C^) =
  val y: A^{x.only[C]} = ???
  runC1(() => { val _ = y; () }) // error

// a local root of a new instance of an unclassified capability class
class K0 extends caps.ExclusiveCapability
def proj(x: Object^): A^{x.only[C]} = ???
def unclassifiedInstance() =
  take(proj(K0())) // error

// fields of a classified class: what remains of `a` is a C2
def localClass(c1: C1^, c2: C2^)(a: A^{c1, c2}) =
  class K extends C1:
    val f: A^{a.except[C1]} = ??? // error
  ()

def mk3(c1: C1^, c2: C2^)(a: A^{c1, c2}): C1^{a.except[C1]} = ???
def exceptVar(c1: C1^, c2: C2^)(a: A^{c1, c2}) =
  val v = mk3(c1, c2)(a) // error
  ()

