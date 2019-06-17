object App {

  trait Nat
  case object Zero extends Nat
  case class Succ[N <: Nat](n: N) extends Nat

  inline def toInt(n: => Nat): Int = inline n match {
    case Zero => 0
    case Succ(n1) => toInt(n1) + 1
  }

  val natTwo = toInt(Succ(Succ(Zero)))

}
