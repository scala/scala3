//> using options -Wunused:all

class A
class B

def Test() =
  val ordA: Ordering[A] = ???
  val ordB: Ordering[B] = ???
  val a: A = ???
  val b: B = ???

  import ordA.given
  val _ = a > a

  import ordB.given
  val _ = b < b

// unminimized OP
trait Circular[T] extends Ordering[T]
trait Turns[C: Circular, T] extends Ordering[T]: // warn Circular is not a marker interface
  extension (turns: T) def extract: C

def f[K, T](start: T, end: T)(using circular: Circular[K], turns: Turns[K, T]): Boolean =
  import turns.given
  if start > end then throw new IllegalArgumentException("start must be <= end")

  import circular.given
  start.extract < end.extract

// -Wunused:implicits warns for unused implicit evidence unless it is an empty interface (only universal members).
// scala 2 also offers -Wunused:synthetics for whether to warn for synthetic implicit params.
object ContextBounds:
  class C[A: Ordered](a: A): // warn
    def f = a

  trait T[A]

  class D[A: T](a: A): // no warn
    def f = a
