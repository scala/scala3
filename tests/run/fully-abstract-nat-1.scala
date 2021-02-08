
object Test {
  def main(args: Array[String]): Unit = {
      println("CaseNums")
      test(CaseNums)
      println()
      println("IntNums")
      test(IntNums)
  }

  def test(numbers: Numbers) = {
    import numbers.*

    val zero: Nat = Zero()
    val one: Nat = Succ(zero)
    val two: Nat = Succ(one)
    val three: Nat = Succ(two)

    zero match {
      case Succ(p) => println("error")
      case Zero() => println("ok")
    }

    one match {
      case Zero() => println("error")
      case Succ(p) => println("ok")
    }

    zero match {
      case s: Succ => println("ok - unchecked error")
      case z: Zero => println("ok - unchecked no error")
    }

    def divOpt(a: Nat, b: Nat): Option[(Nat, Nat)] = b match {
      case s @ Succ(_) =>
        // s is of type Nat though we know it is a Succ
        Some(safeDiv(a, s.asInstanceOf[Succ]))
      case _ => None
    }

    println(divOpt(one, zero))
    println(divOpt(three, two))
  }
}

trait Numbers {

  type Nat
  type Zero <: Nat
  type Succ <: Nat

  val Zero: ZeroExtractor
  trait ZeroExtractor {
    def apply(): Zero
    def unapply(nat: Nat): Boolean
  }

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Nat): Option[Nat]
  }

  implicit def SuccDeco(succ: Succ): SuccAPI
  trait SuccAPI {
    def pred: Nat
  }

  def safeDiv(a: Nat, b: Succ): (Nat, Nat)
}


object CaseNums extends Numbers {

  trait NatClass
  case object ZeroObj extends NatClass
  case class SuccClass(pred: NatClass) extends NatClass

  type Nat = NatClass
  type Zero = ZeroObj.type
  type Succ = SuccClass

  object Zero extends ZeroExtractor {
    def apply(): Zero = ZeroObj
    def unapply(nat: Nat): Boolean = nat == ZeroObj
  }

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = SuccClass(nat)
    def unapply(nat: Nat): Option[Nat] = nat match {
      case SuccClass(pred) => Some(pred)
      case _ => None
    }
  }

  def SuccDeco(succ: Succ): SuccAPI = new SuccAPI {
    def pred: Nat = succ.pred
  }

  def safeDiv(a: Nat, b: Succ): (Nat, Nat) = {
    def sdiv(div: Nat, rem: Nat): (Nat, Nat) =
      if (lessOrEq(rem, b)) (div, rem)
      else sdiv(Succ(div), minus(rem, b))
    sdiv(Zero(), a)
  }

  private def lessOrEq(a: Nat, b: Nat): Boolean = (a, b) match {
    case (Succ(a1), Succ(b1)) => lessOrEq(a1, b1)
    case (Zero(), _) => true
    case _ => false
  }

  // assumes a >= b
  private def minus(a: Nat, b: Nat): Nat = (a, b) match {
    case (Succ(a1), Succ(b1)) => minus(a1, b1)
    case _ => a
  }

}

object IntNums extends Numbers {
  type Nat = Int
  type Zero = Int // 0
  type Succ = Int // n > 0

  object Zero extends ZeroExtractor {
    def apply(): Int = 0
    def unapply(nat: Nat): Boolean = nat == 0
  }

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Int = nat + 1
    def unapply(nat: Nat): Option[Int] =
      if (nat > 0) Some(nat - 1) else None
  }

  def SuccDeco(succ: Succ): SuccAPI = new SuccAPI {
    def pred: Int = succ - 1
  }

  def safeDiv(a: Nat, b: Succ): (Nat, Nat) = (a / b, a % b)
}
