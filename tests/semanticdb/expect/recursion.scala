// semanticdb traversal of annotations of Nat caused an infinite loop
package recursion

object Nats {
  sealed trait Nat {
    transparent inline def ++ : Succ[this.type] = Succ(this)

    transparent inline def +(inline that: Nat): Nat =
      inline this match {
        case Zero    => that
        case Succ(p) => p + that.++
    }
  }

  case object Zero extends Nat
  case class Succ[N <: Nat](p: N) extends Nat

  transparent inline def toIntg(inline n: Nat): Int =
    inline n match {
      case Zero    => 0
      case Succ(p) => toIntg(p) + 1
    }

  val j31 = toIntg(Zero.++.++.++ + Zero.++)
}
