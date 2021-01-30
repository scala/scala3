
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
      case Zero(_) => println("ok") // extra argument removed by language extension
    }

    one match {
      case Zero(_) => println("error") // extra argument removed by language extension
      case Succ(p) => println("ok")
    }

    zero match {
      case s: Succ => println("ok - unchecked error")
      case z: Zero => println("ok - unchecked no error")
    }

    def divOpt(a: Nat, b: Nat): Option[(Nat, Nat)] = b match {
      case s @ Succ(p) =>
        Some(safeDiv(a, s.asInstanceOf[b.type & SuccOpt#Refined])) // safe unchecked cast inserted by the language extension
      case _ => None
    }
    println(divOpt(one, zero))
    println(divOpt(three, two))

    def divOptExpanded(a: Nat, b: Nat): Option[(Nat, Nat)] = {
      val x0 = Succ.unapply(b)
      if (!x0.isEmpty) {
        val s = b.asInstanceOf[b.type & x0.Refined] // safe unchecked cast inserted by the language extension
        val p = x0.get
        Some(safeDiv(a, s))
      } else {
        None
      }
    }
    println(divOptExpanded(one, zero))
    println(divOptExpanded(three, two))
  }
}

trait Numbers {

  type Nat
  type Zero <: Nat
  type Succ <: Nat

  val Zero: ZeroExtractor
  trait ZeroExtractor {
    def apply(): Zero
    def unapply(nat: Nat): ZeroOpt // check that ZeroOpt#Refined <: Nat
  }
  trait ZeroOpt {
    type Refined = Zero // optionally added by language extension
    def get: Null // Language extension should remove this
    def isEmpty: Boolean
  }

  val Succ: SuccExtractor
  trait SuccExtractor {
    def apply(nat: Nat): Succ
    def unapply(nat: Nat): SuccOpt // check that SuccOpt#Refined <: Nat
  }
  trait SuccOpt {
    type Refined = Succ // optionally added by language extension
    def get: Nat
    def isEmpty: Boolean
  }

  implicit def SuccDeco(succ: Succ): SuccAPI
  trait SuccAPI {
    def pred: Nat
  }

  def safeDiv(a: Nat, b: Succ): (Nat, Nat)
}

object CaseNums extends Numbers {

  trait NatClass
  case object ZeroObj extends NatClass with ZeroOpt {
    def get: Null = null // Should be removed by language extension
    def isEmpty: Boolean = false
  }
  case class SuccClass(pred: NatClass) extends NatClass with SuccOpt {
    def get: NatClass = pred
    def isEmpty: Boolean = false
  }

  class EmptyZeroOpt extends ZeroOpt {
    def isEmpty: Boolean = true
    def get: Null = throw new Exception("empty")
  }

  class EmptySuccOpt extends SuccOpt {
    def isEmpty: Boolean = true
    def get: NatClass = throw new Exception("empty")
  }

  type Nat = NatClass
  type Zero = ZeroObj.type
  type Succ = SuccClass

  object Zero extends ZeroExtractor {
    def apply(): Zero = ZeroObj
    def unapply(nat: Nat): ZeroOpt =
      if (nat == ZeroObj) ZeroObj
      else new EmptyZeroOpt
  }

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Succ = SuccClass(nat)
    def unapply(nat: Nat): SuccOpt = nat match {
      case succ: SuccClass => succ
      case _ => new EmptySuccOpt
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
    case (Zero(_), _) => true // extra argument removed by language extension
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
    def unapply(nat: Nat): ZeroOpt = new ZeroOpt {
      def isEmpty: Boolean = nat != 0
      def get: Null = null // language extension will remove this
    }
  }

  object Succ extends SuccExtractor {
    def apply(nat: Nat): Int = nat + 1
    def unapply(nat: Nat): SuccOpt = new SuccOpt {
      def isEmpty: Boolean = nat <= 0
      def get: Int = nat - 1
    }
  }

  def SuccDeco(succ: Succ): SuccAPI = new SuccAPI {
    def pred: Int = succ - 1
  }

  def safeDiv(a: Nat, b: Succ): (Nat, Nat) = (a / b, a % b)
}
