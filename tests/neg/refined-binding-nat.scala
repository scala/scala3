
trait Peano {
  type Nat
  type Zero <: Nat
  type Succ <: Nat

  val Zero: Zero

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Nat): RefinedScrutinee[Succ, Nat] // error
  }
}

object IntNums extends Peano {
  type Nat  = Int
  type Zero = Int
  type Succ = Int

  val Zero: Zero = 0

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = nat + 1
    def unapply(nat: Nat) = // error
      if (nat == 0) RefinedScrutinee.noMatch
      else RefinedScrutinee.matchOf(nat)(nat - 1)
  }

}

object IntNums2 extends Peano {
  type Nat  = Int
  type Zero = Int
  type Succ = Int

  val Zero: Zero = 0

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = nat + 1
    def unapply(nat: Nat): RefinedScrutinee[nat.type & Succ, Nat] =
      if (nat == 0) RefinedScrutinee.noMatch
      else RefinedScrutinee.matchOf(nat)(nat - 1)
  }

}