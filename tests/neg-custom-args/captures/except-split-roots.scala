import language.experimental.captureChecking
import caps.{any, fresh, Classifier, SharedCapability, Control}

trait C   extends SharedCapability, Classifier
trait C1  extends C, Classifier
trait C11 extends C1, Classifier
class A

// Nothing covers the part outside C.
def oneRoot() =
  val x: A^ = A()
  val y: A^{any.only[C]} = x // error

// Nothing covers the hole C1 \ C11.
def hole() =
  val x: A^ = A()
  val y: A^{any.except[C], any.only[C].except[C1], any.only[C11]} = x // error

def take(a: AnyRef^{any.only[C].except[C1], any.only[C11]}): Unit = ()
def classifiedArg(c: C^) = take(c) // error

// A projection of another capability does not cover the rest.
def otherBase(x: A^, z: A^) =
  val y: A^{z.only[C], any.except[C]} = x // error

// The bound admits only parts classified as C.
def run[X^ <: {any.only[C].except[C1], any.only[C1]}](): Unit = ()
def unclassifiedBound(c: Object^) = run[{c}]() // error

// The Control parts of a label that are not ErrHandlers are not covered.
trait ErrHandler extends Control, Classifier
class Lbl extends Control
def handleWith(f: () ->{any.except[Control], any.only[ErrHandler]} Unit): Unit = ()
def label(l: Lbl^): Unit = handleWith(() => println(l)) // error

// Levels: roots of an outer variable cannot hide parts of a local capability.
def levels() =
  var v: A^{any.only[C], any.except[C]} = A()
  def inner(): Unit =
    val z: A^ = A()
    v = z // error

// Result roots unify one-to-one, so a result root is not split across two.
def resultSplit(f: () -> A^{fresh.only[C]}): () -> A^{fresh.only[C].except[C1], fresh.only[C1]} = f // error

class Self(x: A^): // error
  this: Self^{any.only[C]} =>
  def f = x

