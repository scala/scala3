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
trait Turns[C: Circular, T] extends Ordering[T]:
  extension (turns: T) def extract: C

def f[K, T](start: T, end: T)(using circular: Circular[K], turns: Turns[K, T]): Boolean =
  import turns.given
  if start > end then throw new IllegalArgumentException("start must be <= end")

  import circular.given
  start.extract < end.extract
