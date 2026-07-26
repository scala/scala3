import language.experimental.captureChecking

// An explicit self capture set bounds what an implementing class or object may
// capture (the internal view); it is not observed by outside clients. So a bare
// `A[{o}]` instance that captures nothing is pure. See the discussion on #24088.
class A[C^]:
  self: A[{C}]^{C} =>

def test(o: Object^, x: Object^, y: Object^): Unit =
  val a_pure: A[{o}] = A[{o}]                                     // captures nothing => pure
  val a_good = new A[{o}] { def foo() = println(o) }              // ok: {o} within bound {o}
  val b_good = new A[{o, x}] { def foo() = { println(o); println(x) } } // ok
  val c_good: A[{o, x}]^{o, x} = new A[{o, x}] { def foo() = println(x) } // ok: uses x

// A bare `^` on CapSet itself is legal legacy syntax, also through an alias.
type CS0 = caps.CapSet
def viaAlias[C >: CS0 <: CS0^](): Unit = ()
