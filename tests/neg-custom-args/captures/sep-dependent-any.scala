import language.experimental.captureChecking
import language.experimental.separationChecking
import caps.*

// Neg variant of the `sep-dependent-any` regression test: instantiating the
// dependent capture set `{any, f}` with an argument whose capture set is still
// a variable must not crash; the underlying separation errors should be
// reported gracefully.

class Ref extends Mutable:
  private var data: Int = 0
  def get: Int = data
  update def set(x: Int): Unit = data = x

def seq(f: () => Unit, g: () ->{any, f} Unit): Unit = ()

def test1(a: Ref) =
  seq(() => a.set(1), () => ()) // error: cannot call update method on read-only `a`

def test2(a: Ref) =
  seq(() => (), () => a.set(1)) // error: same, in the dependent position

def test3(a: Ref) =
  seq(() => a.set(1), () => a.set(2)) // error // error
