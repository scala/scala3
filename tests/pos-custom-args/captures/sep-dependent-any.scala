import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

// Regression test: a dependent capture set containing `any`, like the
// `{any, f}` below, used to crash CheckCaptures. When the call was rechecked,
// the dependent parameter `f` was substituted by the first argument's capture
// set, which was still a variable. The substitution produced a `Union`
// variable containing `any`, and mapping it with `GlobalCapToLocal` (under
// `withNoVarsMapped`) tripped an assertion in `CaptureSet.map`.

class Ref extends Mutable:
  private var data: Int = 0
  def get: Int = data
  update def set(x: Int): Unit = data = x

def seq(f: () => Unit, g: () ->{any, f} Unit): Unit = ()

def test1() =
  val a = Ref()
  seq(() => a.set(10), () => println(s"value: ${a.get}"))

def test2() =
  val a = Ref()
  seq(() => a.set(10), () => a.set(20)) // both closures capture the same ref

def test3() =
  val a = Ref()
  val b = Ref()
  seq(() => a.set(1), () => b.set(2)) // each closure captures its own ref
  seq(() => (), () => a.set(3))       // first closure captures nothing

def seq3(f: () => Unit, g: () => Unit, h: () ->{any, f, g} Unit): Unit = ()

def test4() =
  val a = Ref()
  seq3(() => a.set(1), () => (), () => a.set(2)) // set depending on two parameters
