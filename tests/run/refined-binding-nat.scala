
object Test {
  def main(args: Array[String]): Unit = {
    app(ClassNums)
    app(IntNums)
  }

  def app(peano: Peano): Unit = {
    import peano.{_, given}
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

  given Typeable[Nat, Zero] {
    def unapply(x: Nat): Option[Zero] = matchZero(x)
  }

  given Typeable[Nat, Succ] {
    def unapply(x: Nat): Option[Succ] = matchSucc(x)
  }

  // given reflect.ClassTag[Succ] = ???

  protected def matchZero(x: Nat): Option[Zero]
  protected def matchSucc(x: Nat): Option[Succ]

  implicit def succDeco(succ: Succ): SuccAPI
  trait SuccAPI {
    def pred: Nat
  }

  val Zero: Zero

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Succ): Option[Nat]
  }
}

object IntNums extends Peano {
  type Nat  = Int
  type Zero = Int
  type Succ = Int

  protected def matchZero(x: Nat): Option[Zero] = if (x == 0) Some(0) else None
  protected def matchSucc(x: Nat): Option[Succ] = if (x != 0) Some(x) else None

  def safeDiv(m: Nat, n: Succ): (Nat, Nat) = (m / n, m % n)

  val Zero: Zero = 0

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = nat + 1
    def unapply(nat: Succ) = Some(nat - 1)
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

  protected def matchZero(x: Nat): Option[Zero] = x match {
    case Zero => Some(Zero)
    case _ => None
  }
  protected def matchSucc(x: Nat): Option[Succ] = x match {
    case nat: SuccClass => Some(nat)
    case _ => None
  }

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
    def unapply(nat: Succ) = Some(nat.predecessor)
  }

  def succDeco(succ: Succ): SuccAPI = new SuccAPI {
    def pred: Nat = succ.predecessor
  }

}
