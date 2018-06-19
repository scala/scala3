
trait Peano {
  type Nat
  type Zero <: Nat
  type Succ <: Nat

  val Zero: Zero

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Nat): SuccOpt // error: missing { def refinedScrutinee: Succ & nat.type }
  }
  trait SuccOpt {
    def isEmpty: Boolean
    def refinedScrutinee: Succ
    def get: Nat
  }
}

object IntNums extends Peano {
  type Nat  = Int
  type Zero = Int
  type Succ = Int

  val Zero: Zero = 0

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = nat + 1
    def unapply(nat: Nat) = new SuccOpt { // error: missing { def refinedScrutinee: Succ & nat.type }
      def isEmpty: Boolean = nat == 0
      def refinedScrutinee: Succ & nat.type = nat
      def get: Int = nat - 1
    }
  }

}

object IntNums2 extends Peano {
  type Nat  = Int
  type Zero = Int
  type Succ = Int

  val Zero: Zero = 0

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = nat + 1
    def unapply(nat: Nat): SuccOpt { def refinedScrutinee: Succ & nat.type } = new SuccOpt {
      def isEmpty: Boolean = nat == 0
      def refinedScrutinee: Succ & nat.type = nat
      def get: Int = nat - 1
    }
  }

}