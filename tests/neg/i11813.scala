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
