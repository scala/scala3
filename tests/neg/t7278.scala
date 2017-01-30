class A { class E }
class B extends A { class EB }
trait C { type E = Int }
trait D { type E = String }
trait EC { type E }

object Test {
  // should not compile (?)
  // martin says "I'd argue about that"
  // martin retracts his statement: this should not compile
  type EE[+X <: EC] = X#E // error: X is not a legal path;
  type EE2[+X <: EC] = X#E // error: X is not a legal path; repeat to get error count to 2

  def fail1(): Unit = {
    val b = new B
    var x1: EE[A] = null // error: Type argument A does not conform to upper bound EC
    var x2: EE[B] = new b.E // error: Type argument B does not conform to upper bound EC
//    x1 = x2  // gives a prior type error: B#E, required: A#E, masked to get at the real thing.
  }

/* Not representable in dotty as there are no existential types
  def fail2(): Unit = {
    val b = new B
    var x1: p.E forSome { val p: A } = new b.E // should not compile
    var x2: p.E forSome { val p: B } = new b.E
    x1 = x2 // should not compile
  }
*/
  def fail3(): Unit = {
    var x1: EE[C] = 5       // error: Type argument C does not conform to upper bound EC
    var x2: EE[C & D] = ""  // error: Type argument C & D does not conform to upper bound EC
    x1 = x2
  }

  def wrap(label: String)(op: => Unit): Unit =
    try { op } catch { case x: ClassCastException => println("%30s %s".format(label, x)) }

  def main(args: Array[String]): Unit = {
    wrap("Variance and inner classes")(fail1())
    wrap("Linearization and type aliases")(fail3())
  }
}
