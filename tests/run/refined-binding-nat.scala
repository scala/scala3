
object Test {
  def main(args: Array[String]): Unit = {
    app(ClassNums)
    app(IntNums)
  }

  def app(peano: Peano): Unit = {
    import peano._
    def divOpt(m: Nat, n: Nat): Option[(Nat, Nat)] = {
      n match {
        case Zero => None
        case s @ Succ(_) => Some(safeDiv(m, s))
      }
    }
    val two = Succ(Succ(Zero))
    val five = Succ(Succ(Succ(two)))
    println(divOpt(five, two))
    println(divOpt(two, five))
    println(divOpt(two, Zero))
  }
}

trait Peano {
  type Nat
  type Zero <: Nat
  type Succ <: Nat

  def safeDiv(m: Nat, n: Succ): (Nat, Nat)

  implicit def succDeco(succ: Succ): SuccAPI
  trait SuccAPI {
    def pred: Nat
  }

  val Zero: Zero

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Nat): RefinedScrutinee[nat.type & Succ, Nat]
  }
}

object IntNums extends Peano {
  type Nat  = Int
  type Zero = Int
  type Succ = Int

  def safeDiv(m: Nat, n: Succ): (Nat, Nat) = (m / n, m % n)

  val Zero: Zero = 0

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = nat + 1
    def unapply(nat: Nat) =
      if (nat == 0) RefinedScrutinee.noMatch
      else RefinedScrutinee.matchOf(nat)(nat - 1)
  }
  def succDeco(succ: Succ): SuccAPI = new SuccAPI {
    def pred: Nat = succ - 1
  }
}

object ClassNums extends Peano {
  trait NatTrait
  object ZeroObject extends NatTrait {
    override def toString: String = "ZeroObject"
  }
  case class SuccClass(predecessor: NatTrait) extends NatTrait

  type Nat  = NatTrait
  type Zero = ZeroObject.type
  type Succ = SuccClass

  def safeDiv(m: Nat, n: Succ): (Nat, Nat) = {
    def intValue(x: Nat, acc: Int): Int = x match {
      case nat: SuccClass => intValue(nat.predecessor, acc + 1)
      case _ => acc
    }
    def natValue(x: Int): Nat =
      if (x == 0) ZeroObject
      else new SuccClass(natValue(x - 1))
    val i = intValue(m, 0)
    val j = intValue(n, 0)
    (natValue(i / j), natValue(i % j))
  }

  val Zero: Zero = ZeroObject

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = new SuccClass(nat)
    def unapply(nat: Nat) = nat match {
      case nat: (SuccClass & nat.type) => RefinedScrutinee.matchOf(nat)(nat.predecessor)
      case _ => RefinedScrutinee.noMatch
    }
  }

  def succDeco(succ: Succ): SuccAPI = new SuccAPI {
    def pred: Nat = succ.predecessor
  }

}
