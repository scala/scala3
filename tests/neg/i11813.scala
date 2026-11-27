trait Ord[A]
given Ord[Int] = new Ord[Int] {}

def test =
  val a = [A] => 1 // error
  val b: [A] => Int = [A] => 1 // error
  val c: [A] => Ord[A] ?=> Int = [A] => 1 // ok, context function is synthesized
  val d = [A] => (x: A) => x // ok
  val e = [A] => { val y = 1; (x: A) => x } // error

  // context bounds
  val f = [A: Ord] => 1 // ok, evidence parameter makes it a context function
  val g: [A: Ord] => Int = [A] => 1 // ok
  val h: [A: Ord] => Int = [A: Ord] => 1 // ok
  val i: [A] => Int = [A: Ord] => 1 // error
  f[Int]
  f[String] // error
  h[String] // error

// no polymorphic eta-expansion
def id[A](x: A): A = x
class Box { def id[A](x: A): A = x }
def mkBox(): Box = Box()

def etaExpansion =
  val f: [A] => A => A = [A] => id[A] // error
  val k: [A] => A => A = [A] => mkBox().id[A] // error
  val l: [A] => Ord[A] ?=> A => A = [A] => id[A] // ok, the context closure comes first

// multiple type parameter clauses
def multiple =
  val g: [A] => [B] => (A, B) => (A, B) = [A] => [B] => (a: A, b: B) => (a, b) // error
  val g1 = [A] => [B] => (a: A, b: B) => (a, b) // error
  val g2 = [A] => [B: Ord] => 1 // error
  val g3: [A] => [B] => Ord[B] ?=> Int = [A] => [B] => 1 // error
  val g4: [A] => Ord[A] ?=> [B] => B => B = [A] => [B] => (b: B) => b // ok
  val g5 = [A: Ord] => [B] => (b: B) => b // ok
  val g6 = [A: Ord] => [B] => 1 // error
  val g7: [A] => Ord[A] ?=> [B] => Ord[B] ?=> Int = [A] => [B] => 1 // ok
