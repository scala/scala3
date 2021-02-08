
import scala.reflect.ClassTag

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
      case s: Succ => println("error") // Will happen with IntNums
      case z: Zero => println("ok")
    }

    def divOpt(a: Nat, b: Nat): Option[(Nat, Nat)] = b match {
      case s @ Succ(_) => Some(safeDiv(a, s))
      case _ => None
    }

    try println(divOpt(one, zero))
    catch { case ex: java.lang.ArithmeticException => println(ex.getMessage) }
    println(divOpt(three, two))
  }
}

trait Numbers {

  type Nat
  type Zero <: Nat
  type Succ <: Nat

  implicit def natTag: ClassTag[Nat]
  implicit def zeroTag: ClassTag[Zero]
  implicit def succTag: ClassTag[Succ]

  val Zero: ZeroExtractor
  trait ZeroExtractor {
    def apply(): Zero
    def unapply(zero: Zero): Boolean
  }

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(succ: Succ): Option[Nat]
  }

  implicit def SuccDeco(succ: Succ): SuccAPI
  trait SuccAPI {
    def pred: Nat
  }

  def safeDiv(a: Nat, b: Succ): (Nat, Nat)
}


object CaseNums extends Numbers {

  trait NatClass
  object ZeroObj extends NatClass { override def toString: String = "ZeroObj" }
  case class SuccClass(pred: NatClass) extends NatClass

  type Nat = NatClass
  type Zero = ZeroObj.type
  type Succ = SuccClass

  def natTag: ClassTag[Nat] = implicitly[ClassTag[NatClass]]
  def zeroTag: ClassTag[Zero] = implicitly[ClassTag[ZeroObj.type]]
  def succTag: ClassTag[Succ] = implicitly[ClassTag[SuccClass]]

  object Zero extends ZeroExtractor {
    def apply(): Zero = ZeroObj
    def unapply(zero: Zero): Boolean = true
  }

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = SuccClass(nat)
    def unapply(succ: Succ): Option[Nat] = Some(succ.pred)
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

  // BROKEN
  // All class tags are identical: Nat, Zero and Succ cannot be distinguished
  def natTag: ClassTag[Int] = ClassTag.Int
  def zeroTag: ClassTag[Int] = ClassTag.Int // will also match any Int that is non zero
  def succTag: ClassTag[Int] = ClassTag.Int // will also match 0

  object Zero extends ZeroExtractor {
    def apply(): Int = 0
    def unapply(zero: Zero): Boolean = true
  }

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Int = nat + 1
    def unapply(succ: Succ): Option[Int] = Some(succ - 1)
  }

  def SuccDeco(succ: Succ): SuccAPI = new SuccAPI {
    def pred: Int = succ - 1
  }

  def safeDiv(a: Nat, b: Succ): (Nat, Nat) = (a / b, a % b)
}
