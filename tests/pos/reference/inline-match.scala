package inlinematch

class Test {

  inline def g(x: Any) <: Any = inline x match {
    case x: String => (x, x) // Tuple2[String, String](x, x)
    case x: Double => x
  }

  val t1: 1.0d = g(1.0d) // Has type 1.0d which is a subtype of Double
  val t2: (String, String) = g("test") // Has type (String, String)

  trait Nat
  case object Zero extends Nat
  case class Succ[N <: Nat](n: N) extends Nat

  inline def toInt(n: Nat) <: Int = inline n match {
    case Zero => 0
    case Succ(n1) => toInt(n1) + 1
  }

  final val natTwo = toInt(Succ(Succ(Zero)))
  val intTwo: 2 = natTwo
}