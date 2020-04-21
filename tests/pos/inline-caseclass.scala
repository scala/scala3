trait Nat
case object Z extends Nat
case class S[N <: Nat](n: N) extends Nat

object Test {
  type Z = Z.type

  transparent inline def add(x: Nat, y: Int): Int = inline x match {
    case Z => y
    case S(x1) => add(x1, y) + 1
  }

  val x = S(S(Z))
  val a: 2 = add(Z, 2)
  inline val y = add(x, 2)
  val z: 4 = y

}

