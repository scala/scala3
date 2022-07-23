trait Z
trait S[N]

type of[A, B] = A & { type T = B }

trait Nat { type T }
case class Zero() extends Nat { type T = Z }
case class Succ[P](prec: Nat of P) extends Nat { type T = S[P] }


type :=:[T, X] = T & { type R = X }

trait :+:[A, B] {
  type R
}

object Addition {
  given AddZero[N]: :+:[Z, N] with {
    type R = N
  }

  given AddSucc[M, N, X](using e: (M :+: N) :=: X): :+:[S[M], N] with {
    val e0: (M :+: N) :=: X = e
    type R = S[X]
  }
}

object Proof {
  import Addition.given

  def zeroAddN[N]: (Z :+: N) :=: N = summon

  def nAddZero[N](n: Nat of N): (N :+: Z) :=: N = n match {
    case Zero() => zeroAddN[Z]
    case p: Succ[pn] =>
      val e0: (pn :+: Z) :=: pn = nAddZero[pn](p.prec)
      AddSucc(using e0)
  }
}

