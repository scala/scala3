import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.{any, Classifier, SharedCapability, ExclusiveCapability}

trait C extends SharedCapability, Classifier
trait K extends ExclusiveCapability, Classifier
trait D extends SharedCapability, Classifier
class A

// The parts of `x` that the roots of the first parameter hide overlap with the second argument.
def f[X^](a: A^{any.only[C], any.except[C]}, b: A^{X}): Unit = ()
def twoArgs() =
  val x: A^ = A()
  f(x, x) // error

// The parts of `x` that the roots of `y` hide cannot be accessed afterwards.
def hideThenUse() =
  val x: A^ = A()
  val y: A^{any.only[C], any.except[C]} = x
  println(x) // error
  println(y)

// Hiding the parts of a parameter needs `consume`, as hiding all of it does.
def hideParam(x: A^) =
  val y: A^{any.only[C], any.except[C]} = x // error

// The split of `x` into `x.only[K]` and `x.only[D]` fails, since the rest of `x` is not
// covered. It is undone, and `x` is then widened to `{k, e}`, so `y` hides only `k`.
def rollback(consume k: K^, e: Object^) =
  val x: A^{k, e} = A()
  val y: A^{any.only[K], any.only[D], e} = x
  println(e) // ok, `e` is not hidden
  println(k) // error
